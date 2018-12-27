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
          navbarPage("App Economia Florestal 0.1.0",id="tab",
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
                     
                     # VPL ####
                     
                     tabPanel("Análise econômica",
                              fluidPage(
                                fluidRow(
                                   h1("Análise econômica", style = "text-align: center;"),
                                   br()),
                                 fluidRow(
                                   
                                   column(3,
                                          
                                          h3("Horizonte de planejamento"),
                                          
                                          
                                          uiOutput("ager"),
                                          
                                          h3("Taxa de juros ao ano"),
                                          
                                          
                                          numericInput( # cria uma lista de opcoes em que o usuario pode clicar
                                            'num.taxa.a.a', # Id
                                            "Insira o valor da taxa de juros ao ano (%)", # nome que sera mostrado na UI
                                            value = "8.75", 
                                            step = 0.1,
                                            min=0,
                                            max=100
                                          ),
                                          DT::dataTableOutput("rawdata"),
                                          br(),
                                          actionButton("runButton", "Rodar"),
                                          h4("Clique no botão após digitar os dados para realizar a análise")
                                          ),
                                   
                                   column(4,
                                          uiOutput("tabt"),
                                          br(),
                                          DT::dataTableOutput("ana_econ_tab")
                                         ),
                                   
                                   column(5,
                                          uiOutput("senst"),
                                          br(),
                                          plotOutput("sens_plot") )
                              )
                              
                              )#fluidPage
                            ),
                     
                     # navbarMenu  Download ####
                     tabPanel("Download",
                              # Painel Download Tabelas ####
                              
                              fluidPage(
                                
                                fluidRow(
                                h1("Downloads", style = "text-align: center;")
                                ),
                                
                                fluidRow(

                                h2("Download de tabelas", style = "text-align: left;")#,
                               # br(),
                                             
                               # helpText(
                               # "Ao clicar no botão de download, você se declara de acordo com os termos descritos",
                               # a(href="https://docs.google.com/document/d/1nvPcNTHCZJhuqsEYoHdYR9NVc44_AJuaHUynQwveVgk/edit?usp=sharing", "aqui"),
                               # "."
                               # )
                                ),
                                             
                                fluidRow(
                                  h3("Para baixar a tabela de resultados, clique no botão abaixo"),
                                  downloadButton('downloadAllData', 'Baixar resultados') 
                                ),
                                
                                fluidRow(

                                  h2("Download de Gráficos", style = "text-align: left;"),
                                  h3("Para baixar o gráfico de sensibilidade, selecione o formato desejado, e clique no botão abaixo"),
                                  selectInput("graphformat",
                                              "Escolha o formato do gráfico:",
                                              choices = c("PNG" = ".png",
                                                          "JPG" = ".jpg",
                                                          "PDF" = ".pdf") ),
                                  
                                  downloadButton('downloadGraph', 'Baixar gráfico')
                                  )
                                
                                
                                             
                                             
                                             
                                    
                                  
                                  
                                     
                              ) # fluidPage
                     ) # final navbarMenu download ####    
                     
                     
                     
                     # final da UI  ####    
          ) # navbarPage
            )#tagList
          ) # ShinyUI



