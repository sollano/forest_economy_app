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
library(ggplot2)
library(ggdendro)
library(ggthemes)
library(openxlsx)
library(rmarkdown)
library(stringr)
library(googledrive)
library(FinCal)

# functions and data ####
source("funs/check_names.R", encoding="UTF-8")
source("funs/renamer.R"    , encoding="UTF-8")
source("funs/vpl_tir.R"    , encoding="UTF-8")
ex1 <- openxlsx::read.xlsx("dados.xlsx")

dig_data_backup <- data.frame(Ano = seq(0, 50,by = 1), Custos = NA_real_, Receitas = NA_real_ )
dig_data <- dig_data_backup
# vectors for names ####

ano_names <- c("Ano","Anos","ANO","ANOS","ano","anos")
custo_names <- c("Custo","Custos","CUSTO","CUSTOS","custo","custos")
receita_names <- c("Receita","Receitas","RECEITA","RECEITAS","receita","receitas")
estratos_names <- c("TALHAO", "Talhao", "talhao","COD_TALHAO","Cod_Talhao","cod_talhao", "COD.TALHAO", "Cod.Talhao","cod.talhao", "area.code", "Area.Code","AREA.CODE", "area_code","Area_Code","AREA_CODE")

# Server ####

shinyServer(function(input, output, session) {
  
  # ui
  output$ager <- renderUI({
    
    sliderInput(inputId = "age_range",
                label = "Selecione o horizonte de planejamento",
                min=0,
                max=50,
                value = c(0,7),
                step = 1 )
    
  })
  
  # Atualizar tabela e dig_data ####
  proxy2 = dataTableProxy('rawdata')
  
  observe({
    invalidateLater(500)
  })  
  
  observeEvent(input$age_range,{
    dig_data <<- dig_data_backup 
    dig_data <<- dig_data[dig_data$Ano %in% input$age_range[1]:input$age_range[2], ]
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
  
  
  
  # Titulo de grafico reativo ####
  
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
      df       = dados,
      ano      = "Ano",
      custo    = "Custos",
      receita  = "Receitas",
      taxa_a_a = input$num.taxa.a.a,
      output   = "full" )
    
    
  })
  
  output$ana_econ_tab <- DT::renderDataTable({
    
    tab <- vplfunc()[[2]] 
    
    as.datatable( formattable(tab,
                              list(
                                Valor = formatter("span",style = x ~ ifelse(x < 0, "color:red", NA))  )
                              ),
               options = list(searching = FALSE,
                              paging=FALSE,
                              ordering=TRUE,
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
    }
    
    # Remover dataframes que geraram errol
    L <- L[!sapply(L, is,"try-error")]
    
    L
    
  })
  list_of_df_all <- reactive({
    
    L <- list()
    
    L[["Analise Economica"]] <- try( vplfunc()[[2]], silent = T) 
    
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
      
      ggsave(file, vplfunc()[[1]], width = 12, height = 10 )
      
      
    }
  )
})









