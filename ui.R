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
                                            fluidRow(  
                                                 column(4, numericInput(inputId="age_start",
                                                                        label = "Inicio",
                                                                        value = 0,
                                                                        min=0,
                                                                        max=50
                                                 ))  ,
                                                 column(4, numericInput(inputId="age_end",
                                                                        label = "Fim",
                                                                        value = 7,
                                                                        min=1,
                                                                        max=100)))   ,
                                          
                                          h3("Taxa de juros ao ano"),
                                          
                                          numericInput(inputId = "num.taxa.a.a",
                                                      label = "Selecione o valor da taxa de juros ao ano (%)",
                                                      min=0,
                                                      max=100,
                                                      value = 8.75,
                                                      step = 0.05 ),
                                          
                                          sliderInput(inputId="sens.lims.slider",
                                                      label = "Selecione os limites da taxa para a análise de sensibilidade (%)",
                                                      min=0,
                                                      max=100,
                                                      value = c(1,30),
                                                      step = 1 ),

                                          DT::dataTableOutput("rawdata"),
                                          br(),
                                          actionButton("runButton", "Rodar"),
                                          h4("Clique no botão após digitar os dados para realizar a análise")
                                          ),
                                   
                                   column(4,
                                          uiOutput("tabt"),
                                          br(),
                                          shinycssloaders::withSpinner(DTOutput("ana_econ_tab"),3,color="#00a90a",color.background="#ffffff")  
                                         ),
                                   
                                   column(5,
                                          uiOutput("senst"),
                                          br(),
                                          shinycssloaders::withSpinner(plotOutput("sens_plot"),3,color="#00a90a",color.background="#ffffff") )
                              )
                              
                              )#fluidPage
                            ),
                     
                     # navbarMenu  Download ####
                     tabPanel("Download",

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
                                  h3("Para baixar a tabela de resultados, clique no botão abaixo:"),
                                  downloadButton('downloadAllData', 'Baixar resultados') 
                                ),
                                
                                fluidRow(

                                  h2("Download de Gráficos", style = "text-align: left;"),
                                  h3("Para baixar o gráfico de sensibilidade, selecione o formato desejado, a resolução (em cm), o dpi, e clique no botão abaixo:"),

                                    column(2,selectInput("graphformat",
                                                         "Formato:",
                                                         choices = c("PNG" = ".png",
                                                                     "JPG" = ".jpg",
                                                                     "PDF" = ".pdf") ) ),
                                    
                                    column(2, numericInput(inputId="width_plot",
                                                           label = "Largura",
                                                           value = 30,
                                                           min=5,
                                                           max=100
                                    ))  ,
                                    column(2, numericInput(inputId="height_plot",
                                                           label = "Altura",
                                                           value = 25,
                                                           min=5,
                                                           max=100)),
                                    
                                    column(2, numericInput(inputId="dpi_plot",
                                                           label = "dpi",
                                                           value = 300,
                                                           min=10,
                                                           max=1000))
                                         ),
                               
                               fluidRow(
                                 downloadButton('downloadGraph', 'Baixar gráfico')
                               )
                                
                                
                                             
                                             
                                             
                                    
                                  
                                  
                                     
                              ) # fluidPage
                     ) # final navbarMenu download ####    
                     
                     
                     
                     # final da UI  ####    
          ) # navbarPage
            )#tagList
          ) # ShinyUI



