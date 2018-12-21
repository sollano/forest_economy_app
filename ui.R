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

shinyUI(
  # Intro, taglists e error messages colors ####
  tagList(tags$style(HTML(".irs-single, .irs-bar-edge, .irs-bar{background: #00a90a}")), # this is actually .css; this changes the color for the sliders
          
          # Timeout: depois de 20 minutos (12000000 milisegundos) fecha a aba do navegador
          tags$script(
            "function idleTimer() {
            var t = setTimeout(logout, 5000);
            window.onmousemove = resetTimer; // catches mouse movements
            window.onmousedown = resetTimer; // catches mouse movements
            window.onclick = resetTimer;     // catches mouse clicks
            window.onscroll = resetTimer;    // catches scrolling
            window.onkeypress = resetTimer;  //catches keyboard actions
            
            function logout() {
            process.exit();  //close the window
            }
            
            function resetTimer() {
            clearTimeout(t);
            t = setTimeout(logout, 12000000);  // time is in milliseconds (1000 is 1 second)
            }
            }
            idleTimer();"
            
          ),
          
          
          # Cor de todas as mensagens da funcao need
          tags$head(
            tags$style(HTML("
                            .shiny-output-error-validation {
                            color: #00a90a;
                            }
                            "))
            ),
          
          # cor das mensagens que eu especificar com "WRONG"
          tags$head(
            tags$style(HTML("
                            .shiny-output-error-WRONG {
                            color: red;
                            }
                            "))
            ),
          
          # cor das mensagens que eu especificar com "AVISO"
          tags$head(
            tags$style(HTML("
                            .shiny-output-error-AVISO {
                            color: orange;
                            }
                            "))
            ),
          
          
          
          # Version ####
          navbarPage("App Economia Florestal 0.0.2",id="tab",
                     #         ####           
                     theme = "green_yeti2.css",
                     # theme = "green.css", # seleciona um tema contido na pasta www
                     # theme = shinythemes::shinytheme("paper"), # seleciona um tema utilizando pacote
                     
                     # Painel Intro ####          
                     tabPanel( "Intro" ,
                               fluidRow(
                                 column(5,
                                        includeMarkdown("about.md")
                                 ),
                                 column(6,
                                        img(contentType = "image/jpg",
                                            src="intro_picture.jpg",
                                            width = 770,
                                            #           height = 750)
                                            height = 856)
                                        
                                 )
                               ) # fluid row
                     ), # Painel Intro             
                     
                     
                     # Upload de dados ####
                     tabPanel("Importação",
                              sidebarLayout(
                                
                                sidebarPanel(
                                  
                                  h3("Dados"),
                                  
                                  radioButtons("df_select", 
                                               "Fazer o upload de um arquivo, ou utilizar o dado de exemplo?", 
                                               c("Fazer o upload",
                                                 "Digitar dados",
                                                 "Utilizar o dado de exemplo" ), 
                                               selected = "Fazer o upload"),
                                  
                                  uiOutput("upload"), # tipos de arquivos aceitos
                                  hr(),
                                  uiOutput("upload_csv"), # tipos de arquivos aceitos
                                  uiOutput("upload_xlsx"), # tipos de arquivos aceitos
                                  uiOutput("age_range")
                                  
                                ), # sidebarPanel
                                
                                mainPanel(
                                  DT::dataTableOutput("typed_data"),
                                  DT::dataTableOutput("rawdata")
                                ) # mainPanel
                              ) # sidebarLayout
                     ),
                     
                     # Mapeamento ####
                     tabPanel("Mapeamento de variáveis",
                              fluidPage(
                                
                                #h1("Shiny", span("Widgets Gallery", style = "font-weight: 300"), 
                                h1("Definição dos nomes das variáveis", 
                                   style = "text-align: center;"),
                                br(),
                                
                                #  h4("Nesta aba serão indicados os nomes das colunas que serão utilizadas nas análises em todo o app"),
                                
                                fluidRow( # fluidRow 1 start
                                  column(4,
                                         wellPanel(
                                           h3("Ano*"),
                                           p("Selecione o nome da variável referente à 'Ano':"#, 
                                             #style = "font-family: 'Source Sans Pro';"
                                           ),
                                           uiOutput("selec_ano")
                                         )), # Coluna ano
                                  
                                  
                                  column(4,
                                         wellPanel(
                                           h3("Custos*"),
                                           p("Selecione o nome da variável referente à 'Custos':"#, 
                                             #style = "font-family: 'Source Sans Pro';"
                                           ),
                                           uiOutput("selec_custo")
                                         )), # coluna custo
                                  
                                  column(4,
                                         wellPanel(
                                           h3("Receitas*"),
                                           p("Selecione o nome da variável referente à 'Receitas':"#, 
                                             #style = "font-family: 'Source Sans Pro';"
                                           ),
                                           uiOutput("selec_receita")
                                         )) # Coluna receita,
                                  
                                  
                                ), # fluidRow 1 end
                                
                                fluidRow(  # fluidRow 2 start 
                                  
                                  column(4,
                                         wellPanel(
                                           uiOutput("selec_taxa.a.a_num")
                                         ))#, # Coluna taxa
                                  
                             #     column(4,
                             #            wellPanel(
                             #              h3("Estrato"),
                              #             p("Selecione o nome da variável referente à 'Estrato'"#, 
                              #               #style = "font-family: 'Source Sans Pro';"
                              #             ),
                              #             uiOutput("selec_estrato")
                              #           ))                       
                                  

                                ) # fluidRow 2 end

                              ) # fluidPage 
                              
                              
                              ),# tabPanel Mapeamento
                     
                     
   
                     
                     # VPL ####
                     
                     tabPanel("Análise econômica",
                              fluidPage(
                                
                                h1("Análise econômica", style = "text-align: center;"),
                                br(),
                                
                                DT::dataTableOutput("ana_econ_tab")
                              )
                            ),
                     
                     # navbarMenu  Download ####
                     tabPanel("Download",
                              # Painel Download Tabelas ####
                              
                              fluidPage(
                                
                                
                                h1("Download dos resultados", style = "text-align: center;"),
                                br(),
                                
                                
                                tabsetPanel(
                                  tabPanel("Download de tabelas", 
                                           fluidPage(
                                             
                                             
                                             h2("Download de tabelas", style = "text-align: center;"),
                                             br(),
                                             
                                             helpText(
                                               "Ao clicar no botão de download, você se declara de acordo com os termos descritos",
                                               a(href="https://docs.google.com/document/d/1nvPcNTHCZJhuqsEYoHdYR9NVc44_AJuaHUynQwveVgk/edit?usp=sharing", "aqui"),
                                               "."
                                             ),
                                             
                                             fluidRow(
                                               column(
                                                 10
                                                 ,uiOutput("checkbox_df_download")
                                               )
                                               
                                             ),
                                             br(),
                                             
                                             fluidRow(column(3,downloadButton('downloadData', 'Baixar tabelas selecionadas'), offset=4)),
                                             br(),
                                             
                                             h3("Ou, para baixar todas as tabelas disponíveis, clique abaixo:"),
                                             fluidRow(
                                               column(3,downloadButton('downloadAllData', 'Baixar todas as tabelas'), offset=4)
                                             )
                                             
                                             
                                             
                                           )
                                  ) # download tabelas
                                  
                                )       
                              ) # fluidPage
                     ) # final navbarMenu download ####    
                     
                     
                     
                     # final da UI  ####    
          ) # navbarPage
            )#tagList
          ) # ShinyUI



