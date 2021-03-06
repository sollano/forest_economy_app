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
  
  # Importação ####
  
  #ui
  output$upload      <- renderUI({
    
    validate(need(input$df_select == "Fazer o upload", "" )  )
    
    list(   
      
      radioButtons("df_extension", 
                   "Informe o formato do arquivo:", 
                   choices = c(".csv (Valor separado por virgulas) ou .txt (arquivo de texto)",
                               ".xlsx (Excel)"),
                   selected =".xlsx (Excel)")
    )
  })
  output$upload_csv  <- renderUI({
    
    validate(need(input$df_select == "Fazer o upload" & input$df_extension == ".csv (Valor separado por virgulas) ou .txt (arquivo de texto)", "" )  )
    
    list(    
      
      radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
        inputId='sep',  #Id
        label='Separador:', # nome que sera mostrado na UI
        choices=c(Virgula=',', "Ponto e Virgula"=';', Tabulação='\t'), # opcoes e seus nomes
        selected=','), # valor que sera selecionado inicialmente
      
      radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
        inputId='dec', # Id
        label='Decimal:', # nome que sera mostrado na UI
        choices=c(Ponto=".", Virgula=","), # opcoes e seus nomes
        selected="."), # valor que sera selecionado inicialmente
      
      fileInput( # input de arquivos
        inputId = "file1", # Id
        
        label = "Selecione o arquivo: (.csv ou .txt)", # nome que sera mostrado na UI
        
        accept=c('text/csv', ".txt",'.csv'))
    )
    
    
  })
  output$upload_xlsx <- renderUI({
    
    validate(need(input$df_select == "Fazer o upload" & input$df_extension == ".xlsx (Excel)", "" )  )
    
    list(    
      # Selecionar numero da planilha
      numericInput(inputId = "sheet_n",
                   label   = "Número da planilha",
                   value   = 1,
                   min     = 1,
                   max     = 30,
                   step    = 1
      ),
      
      #radioButtons(inputId = "mv_excel",label = "Valores ausentes", choices = c("Espaço vazio" = "", "NA" = "NA"), inline = T ),
      
      # input de arquivos
      fileInput( 
        inputId = "file2", # Id
        
        label = "Selecione o arquivo: (.xlsx)", # nome que sera mostrado na UI
        
        # So aceita .xlsx
        accept=c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                 '.xlsx'))#,
      
      
      #div("Recomendamos o uso do formato .csv", style = "color:blue")
      
      
    )
    
    
  })
  output$age_range   <- renderUI({
    
    validate(need(input$df_select == "Digitar dados", "" )  )
    
    sliderInput(inputId = "age_range",
                label = h3("Selecione o horizonte de planejamento"),
                min=0,
                max=50,
                value = c(0,7),
                step = 1 )
    
  })
  
  #tabela
  upData <- reactive({ # Criamos uma nova funcao reactive. este sera o objeto filtrado, utilizado nos calculos
    # sera vazio caso nao seja selecionado "fazer o upload"
    validate(
      need(input$df_select, ""),
      need(input$df_extension, ""),
      need(input$df_select == "Fazer o upload" , "" )  )
    
    # Salva o caminho do arquivo uploadado em um arquivo, dependendo do que o usuario selecionar
    if(input$df_extension == ".csv (Valor separado por virgulas) ou .txt (arquivo de texto)"){
      inFile <- input$file1
    }else if( input$df_extension == ".xlsx (Excel)"){
      inFile <- input$file2
    } # caso contrario, salvar o caminho do arquivo carregado em inFile
    
    # input$file1 sera NULL inicialmente. apos o usuario selecionar
    # e upar um arquivo, ele sera um data frame com as colunas
    # 'size', 'type', e 'datapath' . A coluna 'datapath' 
    # ira conter os nomes dos arquivos locais onde o dado pode ser encontrado
    
    # precisa do caminho do dado pra rodar os codigos a seguir
    req(inFile)
    
    if(input$df_extension != ".xlsx (Excel)")
    {
      raw_data <- read.csv(inFile$datapath, header=TRUE, sep=input$sep, dec=input$dec,quote='"')
    } else {
      file.copy(inFile$datapath,
                paste(inFile$datapath, "xlsx", sep="."))
      raw_data <-  readxl::read_xlsx(paste(inFile$datapath, "xlsx", sep="."), input$sheet_n, na = c("","NA")) 
      #raw_data <-  openxlsx::read.xlsx(paste(inFile$datapath, "xlsx", sep="."), input$sheet_n) 
      raw_data <- as.data.frame(raw_data)
    }
    
    raw_data # tabela final a ser mostrada. 
    
  })

  # Fazer a tabela ser editaval apenas quando digitar dados for selecionado ####
  edit_boolean <- reactiveValues(edit=FALSE)

  observe({
    req(input$df_select=="Digitar dados")
    edit_boolean$edit <- TRUE
  })
  
  observe({
    req(input$df_select!="Digitar dados")
    edit_boolean$edit <- FALSE
  })
  # rawData_ ####
  # rawData_ (com traco) sera o dado bruto sem filtro. Este dataframe sera utilizado em todo o app
  rawData_ <- reactive({
    # raw data, sera definido como o exemplo, ou o dado de upload, dependendo do usuario.
    # para evitar erros, caso seja selecionado "Fazer o upload" mas o dado ainda não tenha sido uploadado,
    # sera retornanado vazio
    switch(input$df_select, 
           "Digitar dados" = if(is.null(input$age_range)){return()}else{dig_data},
           "Fazer o upload" = if(is.null(input$file1) && is.null(input$file2)){return()}else{upData()},
           "Utilizar o dado de exemplo" = ex1 )
  })
  
  # render table
  output$rawdata <- DT::renderDataTable({ # renderizamos uma DT::DataTable
    
    validate(
      need(!is.null(rawData_()), "")#,
      )
    
    # salvamos a funcao rawData_, que contem o arquivo carregado pelo usuario em um objeto
    data <- rawData_() 
    
    datatable(data,
              editable = edit_boolean$edit,
              class = 'cell-border stripe',
              rownames = FALSE,
              options = list(
                processing = FALSE,
                searching = FALSE,
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
  
  # Atualizar tabela e dig_data ####
  proxy2 = dataTableProxy('rawdata')
  
  observeEvent(input$age_range,{
    dig_data <<- dig_data_backup 
    dig_data <<- dig_data[dig_data$Ano %in% input$age_range[1]:input$age_range[2], ]
  })
  
  observeEvent(input$rawdata_cell_edit, {
    info = input$rawdata_cell_edit
    i = info$row
    j = info$col + 1  # column index offset by 1
    v = info$value
    dig_data[i, j] <<- DT::coerceValue(v, dig_data[i, j])
    replaceData(proxy2, dig_data, resetPaging = FALSE, rownames = FALSE)  # important
    #print(dig_data)
    #print(str(dig_data))
  })
  
  # send data ####
  send_sheet <- reactive({
    
    validate(need( !is.null(upData()) , "" )  )
    
    #pegar os nomes
    varnames <- varnames()
    
    # Cria um dataframe com os nomes padronizados das variaveis mapeadas
    df_up <- renamer(upData(), 
                     ano = varnames$ano,
                     custo = varnames$custo,
                     receita = varnames$receita,
                     taxa.a.a = varnames$taxa.a.a
                     
                     )
    # Faz login na conta do google usando o token
    #suppressMessages(googlesheets::gs_auth(token = "googlesheets_token.rds",verbose = FALSE))
    
    # Manda o arquivo para a conta da google, no google spreadsheets
    #googlesheets::gs_new(title=paste(round(abs(rnorm(1,1,1)),2),"nat_app", Sys.Date(),format(Sys.time(), "%H_%M_%S"),sep = "_"),input = df_up,trim = FALSE,verbose = FALSE)
    
    #login
    suppressMessages(drive_auth("googlesheets_token.rds",verbose = F))
    #print("logged in")
    #nome do arquivo
    fn <-paste(Sys.Date(),format(Sys.time(),"%H_%M_%S"),round(abs(rnorm(1,1,1)),2),"econ_app",".csv",sep = "_")
    
    # salva arquivo temporario no disco
    write.csv(df_up,file = fn,row.names = FALSE)
    
    # manda pro drive
    suppressMessages(drive_upload(fn, paste("ForestEconomyApp",fn,sep="/"),verbose = F))
    #print("file uploaded")
    # delete arquivo temporario
    unlink(fn)
    
    # deleta objeto fn
    rm(fn)
    
    
    
  })
  
  # dummy observer for linux (makes session flush when a download is made)
  observe({
    invalidateLater(500)
  })  
  observe({
    #print(rnDownloads$ndown)
    # So rodar se algum dado for uploadado
    req( !is.null(upData()) )
    # Se algum botao de download for clicado, enviar dados para a nuvem
    req(rnDownloads$ndown>0)
    #send_sheet()
  })
  
  
  
  
  
  # Mapeamento ####
  
  # ui
  output$selec_ano          <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.ano", # Id
      NULL,
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = ano_names,
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    # obs: multiple = T & maxItems = 1, garantem que a celula fique vazia, caso o app falhe
    # em tentar adivinhar o nome da especie
  })
  output$selec_custo        <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.custo", # Id
      NULL,
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = custo_names,
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    # obs: multiple = T & maxItems = 1, garantem que a celula fique vazia, caso o app falhe
    # em tentar adivinhar o nome da especie
  })
  output$selec_receita      <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.receita", # Id
      NULL,
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = receita_names,
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    # obs: multiple = T & maxItems = 1, garantem que a celula fique vazia, caso o app falhe
    # em tentar adivinhar o nome da especie
  })
  output$selec_estrato      <- renderUI({
    
    data <- rawData_()
    
    selectizeInput("col.estrato",
                   NULL, # nome que sera mostrado na UI
                   choices = names(data),
                   selected = estratos_names,
                   multiple = T,
                   options = list(
                     maxItems = 10,
                     placeholder = 'Selecione uma coluna abaixo:'#,
                     #    onInitialize = I('function() { this.setValue(""); }')
                   ) # options    
    )# selectize
    
  })
  output$selec_taxa.a.a_num <- renderUI({
    
    list(
      
      h3("Taxa de juros ao ano*"),
      
      
      numericInput( # cria uma lista de opcoes em que o usuario pode clicar
        'num.taxa.a.a', # Id
        "Insira o valor da taxa de juros ao ano (%)", # nome que sera mostrado na UI
        value = "8.75", 
        step = 0.1,
        min=0,
        max=100
      )
      
    )
    
  })
  
  # Set names ####
  varnames <- reactive({
    
    varnameslist <- list(
      
      ano   = input$col.ano,
      custo = input$col.custo,
      receita = input$col.receita,
      taxa.a.a = input$num.taxa.a.a
    )
    
    # esses ifs garantem que se o usuario digitar os dados,
    # ele nao precisara de mapear variaveis, e nao precisara
    # clicar na aba de mapeamento, indo direto para a aba de calculo
    if(is.null(varnameslist$ano) & input$df_select == "Digitar dados"){
      varnameslist$ano <- "Ano"
    }
    
    if(is.null(varnameslist$custo) & input$df_select == "Digitar dados"){
      varnameslist$custo <- "Custos"
    }
    
    if(is.null(varnameslist$receita) & input$df_select == "Digitar dados"){
      varnameslist$receita <- "Receitas"
    }
    
    
    x <- lapply(varnameslist, function(x){if(is.null(x)){x<-""}else{x} } )   
    
    x
  })
  
  output$teste <- renderTable({
    varnames()
    
  })
  
  
  # Final Switch ####
  readyData <- reactive({
    # Ready data e um reactive separado.
    # por algum motivo, dig_data nao estava atualizando dentro de rawData_,
    # entao tive que fazer esse readyData, que e basicamente um repeteco de rawData_
    switch(input$df_select, 
           "Fazer o upload" = if(is.null(input$file1) && is.null(input$file2)){return()}else{rawData_()},
           "Digitar dados" = if(is.null(input$age_range)){return()}else{dig_data},
           "Utilizar o dado de exemplo" = ex1 )
  })
  
  # VPL ####
  
  tabvpl <- reactive({
    
    
    nm <- varnames()
    dados <- readyData()
    
    validate(
      need(dados, "Por favor faça o upload da base de dados"),
      need(nrow(dados)>0, "Base de dados vazia"),
      need(nm$ano,"Por favor mapeie a coluna referente a 'Ano'  "),
      need(nm$custo,"Por favor mapeie a coluna referente a 'Custos' "),
      need(nm$receita,"Por favor mapeie a coluna referente a 'Receitas' "),
      need(nm$taxa.a.a,"Por favor mapeie a coluna referente a 'Taxa de juros ao ano' ")
    )
    
    vpl_tir(
      df       = dados,
      ano      = nm$ano,
      custo    = nm$custo,
      receita  = nm$receita,
      taxa_a_a = nm$taxa.a.a,
      output   = "simple" )
    
    
  })
  
  output$ana_econ_tab <- DT::renderDataTable({
    
    tab <- tabvpl() 
    
    as.datatable( formattable(tab,
                              list(
                                VPL = formatter("span",style = x ~ ifelse(x < 0, "color:red", NA)) ,
                                VFL = formatter("span",style = x ~ ifelse(x < 0, "color:red", NA)),
                                BC = formatter("span",style = x ~ ifelse(x < 0, "color:red", NA)),
                                VET = formatter("span",style = x ~ ifelse(x < 0, "color:red", NA)),
                                VPLA = formatter("span",style = x ~ ifelse(x < 0, "color:red", NA)),
                                TIR = formatter("span",style = x ~ ifelse(x < 0, "color:red", NA)) )
                              ),
               options = list(searching = FALSE,
                              paging=FALSE,
                              ordering=TRUE,
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                "}")
               )   
    ) 
    
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
      L[["Analise Economica"]] <- try( tabvpl(), silent = T) 
    }
    
    # Remover dataframes que geraram errol
    L <- L[!sapply(L, is,"try-error")]
    
    L
    
  })
  list_of_df_all <- reactive({
    
    L <- list()
    
    L[["Analise Economica"]] <- try( tabvpl(), silent = T) 
    
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
  
  
})









