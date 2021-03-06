setwd("/home/ubuntu/crypto/")#
#setwd("C:/Users/Po/Sync/Crypto/dashboard")
library(shiny)
library(shinydashboard)

shinyUI(fluidPage(
  # tags$head(tags$script(src="adsense.js")),
  
  tags$style("              body {
             -moz-transform: scale(0.8, 0.8); /* Moz-browsers */
             zoom: 0.8; /* Other non-webkit browsers */
             zoom: 80%; /* Webkit browsers */      }       "),
  
  dashboardPage(
                dashboardHeader(title="Crypto Dashboard",
                                dropdownMenu(
                                  type="message", messageItem(from="Dashboard welcome", message="Welcome to omega build."),                      # messageItem(from = "Sales Update", message = "Sales are at 55%", icon=icon("bar-chart"), time="22:00"),                      # messageItem(from = "Sales Udpate", message = "Meeting at 6pm", icon=icon("handshake-o"), time="03-25-2017")
                                  messageItem(from="Dashboard roadmap: A", message="Ads and donation links"),
                                  messageItem(from="Dashboard roadmap: B", message="NLP scores not yet included in models"),
                                  messageItem(from="Dashboard roadmap: C", message="ml prediction models: neural net, RNN, bayesTS")       )),
                
                dashboardSidebar(
                  sidebarMenu(
                    menuItem("Dashboard", tabName='dashboard', icon=icon('dashboard')),
                    #selectInput('exchangeInputA', label='Exchange A', choices=c('binance'), selected = 'binance'),  #, 'bitfinex'
                    
                    uiOutput("pair.controlsA"),
                    numericInput('pair.timeA', label='Time period A', value=30, min=15, max=150),
                    br(),br(),
                    #selectInput('exchangeInputB', label='Exchange B', choices=c('binance'), selected = 'binance'),  #, 'bitfinex')
                    uiOutput("pair.controlsB"),    
                    numericInput('pair.timeB', label='Time period B', value=30, min=15, max=150),
                    menuItem("Documentation", tabName = 'Documentation'),
                    menuItem("Donate", tabName = 'Donate')
                  )),
                
                dashboardBody(    #  mainPanel(tags$head(tags$style(type="text/css", ".tab-content {overflow: visible;}"))),
                  tabItems(
                    tabItem(tabName = "dashboard",  
                            fluidRow(
                              box(status='primary', width=6, solidHeader = T, uiOutput("pair.varsA") ),
                              box(status='primary', width=6, solidHeader = T, uiOutput("pair.varsB") )      ),
                            fluidRow(                  
                              box(status="primary", solidHeader = F, plotOutput("plot1"), textOutput('textout1') ),
                              box(status="primary", solidHeader = F, plotOutput("plot2"), textOutput('textout2') ) ),
                            fluidRow(                                 
                              box(status="primary", width=6, solidHeader = F, uiOutput("modellistA"), actionButton('modelActionA', label='Predict'), tableOutput('modelTableA')  ),
                              box(status="primary", width=6, solidHeader = F, uiOutput("modellistB"), actionButton('modelActionB', label='Predict'), tableOutput('modelTableB') ) ),
                            fluidRow(
                              box(status="primary", solidHeader = F, plotOutput("NLPplot1") ),
                              box(status="primary", solidHeader = F, plotOutput("NLPplot2") ) )
                    ),
                    tabItem(tabName = "Documentation", 
                            pre(includeText("documentation.txt"))
                    ),
                    tabItem(tabName = "Donate",
                            pre(includeText("donate.txt"))
                    )
                  )
                )
  )        
  )
)