options(shiny.sanitize.errors = FALSE)
library(shiny)
suppressPackageStartupMessages(library(DT))
#library(plotly)
library(formattable)
library(readxl)
#library(plyr)
library(tibble)
library(tidyr)
suppressPackageStartupMessages(library(dplyr))
library(lazyeval)
suppressPackageStartupMessages(library(ggplot2))
library(ggdendro)
library(ggthemes)
library(openxlsx)
library(rmarkdown)
library(stringr)
library(googledrive)
library(FinCal)
library(shinycssloaders)
library(googlesheets)
library(rgeolocate)

# functions and data ####
source("funs/check_names.R", encoding="UTF-8")
#source("funs/renamer.R"    , encoding="UTF-8")
source("funs/vpl_tir.R"    , encoding="UTF-8")
#ex1 <- openxlsx::read.xlsx("dados.xlsx")

dig_data_backup <- data.frame(Ano = seq(0, 100,by = 1), Custos = NA_real_, Receitas = NA_real_ )
dig_data <- dig_data_backup
# vectors for names ####

ano_names <- c("Ano","Anos","ANO","ANOS","ano","anos")
custo_names <- c("Custo","Custos","CUSTO","CUSTOS","custo","custos")
receita_names <- c("Receita","Receitas","RECEITA","RECEITAS","receita","receitas")
estratos_names <- c("TALHAO", "Talhao", "talhao","COD_TALHAO","Cod_Talhao","cod_talhao", "COD.TALHAO", "Cod.Talhao","cod.talhao", "area.code", "Area.Code","AREA.CODE", "area_code","Area_Code","AREA_CODE")

# Server ####

shinyServer(function(input, output, session) {
  
  # logging ####
  
  # once=TRUE resolve o problema de postar duas vezes
  observeEvent(input$ipid,once=TRUE,eventExpr={
    
    # add require pra so rodar quando conseguir o ip
    req(input$ipid!="")
    
    fingerprint <- input$fingerprint
    ipid <- input$ipid
    
    suppressMessages(gs_auth("googlesheets_token.rds",verbose = F))
    
    # pega informacoes com base no ip
    result <- rgeolocate::ip_api(input$ipid)
    #result <- rgeolocate::ip_api("186.244.182.177")
    
    # converter data pro timezone correto
    systime <- lubridate::with_tz(Sys.time(), tzone = result$timezone)
    
    # add informacoes
    
    result <- result %>% 
      mutate(
        app= "App Economia Florestal",
        ip = input$ipid,
        hash = input$fingerprint,
        data = format(systime, "%d/%m/%Y"),
        dia = format(systime, "%d"),
        mes = format(systime, "%B"),
        ano = format(systime, "%Y"),
        hora=format(systime, "%X") ) %>% 
      select(app,ip,data,hora,region_name,region_code,country_code,isp,latitude,longitude,organisation,timezone,zip_code,status,hash,dia,mes,ano)
    
    gs_add_row(gs_title("app_logs",verbose=FALSE), 
               ws = 1,
               input = result,
               verbose = FALSE)
    
  })
  
  # Atualizar tabela e dig_data ####
  proxy2 = dataTableProxy('rawdata')
  
  observeEvent(input$age_end,{
    
    if(is.null(input$age_end)|is.na(input$age_end)|input$age_end==""){
      age.end <- 1
    }else{
      age.end <- input$age_end
    }
    
    dig_data <<- dig_data_backup 
    dig_data <<- dig_data[dig_data$Ano %in% 0:age.end, ]

    # Replace data que realmente atualiza o data frame... sem ele nao funciona
    replaceData(proxy2, dig_data, resetPaging = FALSE, rownames = FALSE)  # important
    
    
  }, priority = 2)
  
  observeEvent(input$rawdata_cell_edit, {
    info = input$rawdata_cell_edit
    i = info$row
    j = info$col + 1  # column index offset by 1
    v = info$value
    dig_data[i, j] <<- DT::coerceValue(v, dig_data[i, j])
    replaceData(proxy2, dig_data, resetPaging = FALSE, rownames = FALSE)  # important
    #print(dig_data)
    #print(str(dig_data))
  }, priority = 1)
  
  # render editable table ####
  output$rawdata <- DT::renderDataTable({ # renderizamos uma DT::DataTable
    
    datatable(dig_data,
              editable = TRUE,
              class = 'cell-border stripe',
              rownames = FALSE,
              options = list(
                processing = FALSE,
                searching = FALSE,
                ordering = FALSE,
                info = FALSE,
                paging=FALSE,
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                  "}"),
                pageLength = 25
              )
    ) # Criamos uma DT::datatable com base no objeto
    
    # Este arquivo e reativo, e ira se alterar caso o usuario
    # aperte o botao input$columns
    
  })
  
  
  
  # Titulos reativos ####
  
  output$tabt <- renderUI({
    
    req(input$runButton)
    
    h3("Resultado da análise", style = "text-align: center;")
    
  })
  output$senst <- renderUI({
    
    req(input$runButton)
    
    h3("Análise de sensibilidade", style = "text-align: center;")
    
  })
  
  # VPL ####
  
  vplfunc <- eventReactive(input$runButton,{
    
    dados <- dig_data
    
    #req(!all(is.na(dados$Custos)))
    #req(!all(is.na(dados$Receitas)))
    
    vpl_tir(
      df          = dados,
      ano         = "Ano",
      custo       = "Custos",
      receita     = "Receitas",
      taxa_a_a    = input$num.taxa.a.a,
      output      = "full",
      sens_limits = input$sens.lims.slider )
    
    
  })
  
  output$ana_econ_tab <- DT::renderDT({
    
    tab <- vplfunc()[[2]] 
    
    as.datatable( formattable(tab,
                              list(
                                Valor = formatter("span",style = x ~ ifelse(x < 0, "color:red", NA))  )
                              ),
               options = list(searching = FALSE,
                              paging=FALSE,
                              ordering=FALSE,
                              info = FALSE,
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                "}")
               )   
    ) 
    
  }) 
  output$sens_plot <- renderPlot({
    
    
    vplfunc()[[1]] 
    
  })
  
  
  # Download tabelas ####
  
  # Cria um valor inicial zero para verificar se o usuario fez algum download ou nao.
  # Se o usuario clicar em algum botao de download, sera add a esse valor uma unidade.
  rnDownloads <- reactiveValues(ndown=0)
  
  output$checkbox_df_download <- renderUI({
    
    checkboxGroupInput("dataset", h3("Escolha uma ou mais tabelas, e clique no botão abaixo:"), 
                       choices =  c(
                         "Analise Economica"                     
                       ), inline = T )
    
    
  })
  
  list_of_df_to_download <- reactive({
    
    L <- list()
    
    if("Analise Economica" %in% input$dataset ) {
      L[["Analise Economica"]] <- try( vplfunc()[[2]], silent = T) 
      L[["Analise Economica"]][,2] <- try(as.numeric(L[["Analise Economica"]][,2]), silent=T)
      
    }
    
    # Remover dataframes que geraram errol
    L <- L[!sapply(L, is,"try-error")]
    
    L
    
  })
  list_of_df_all <- reactive({
    
    L <- list()
    
    L[["Analise Economica"]] <- try( vplfunc()[[2]], silent = T) 
    L[["Analise Economica"]][,2] <- try(as.numeric(L[["Analise Economica"]][,2]), silent=T)
    
    # Remover dataframes que geraram errol
    L <- L[!sapply(L, is,"try-error")]
    
    L
    
  })
  output$downloadData <- downloadHandler(
    filename = function(){"tabelas_app_economia_forest.xlsx"},
    
    content = function(file){
      rnDownloads$ndown <- rnDownloads$ndown + 1
      suppressWarnings(openxlsx::write.xlsx( list_of_df_to_download(), file ))}
    
  )
  
  output$downloadAllData <- downloadHandler(
    filename = function(){"tabelas_app_economia_forest.xlsx"},
    
    content = function(file){ 
      rnDownloads$ndown <- rnDownloads$ndown + 1
      suppressWarnings(openxlsx::write.xlsx( list_of_df_all(), file )) }
    
  )
  
  
  
  # Download plot ####
  
  output$downloadGraph <- downloadHandler(
    filename = function() { 
      
      if(input$graphformat==".png")
      {
        paste("sensibility_plot", '.png', sep='') 
      }
      else if(input$graphformat==".jpg")
      {
        paste("sensibility_plot", '.jpg', sep='') 
      }
      else if(input$graphformat==".pdf")
      {
        paste("sensibility_plot", '.pdf', sep='') 
      }
      
    },
    
    content = function(file) {
      rnDownloads$ndown <- rnDownloads$ndown + 1
      
      ggsave(file, vplfunc()[[1]], width = input$width_plot, height = input$height_plot, dpi = input$dpi_plot, units="cm" )
      
      
    }
  )
})









