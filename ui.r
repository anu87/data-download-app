library(shiny)


shinyUI(fluidPage(
  tags$style(".container { border:2px solid steelblue; width: 100%; height: 450px; overflow-y: scroll; }"),
  titlePanel("Truncate PSNU/Site by IM Factview data"),
  
  fluidRow(
    column(4, 
           wellPanel(
             actionButton('selectall', 'Select All'),
             uiOutput('choose_col')
           )
    ),
    column(8,
           wellPanel(fluidRow(
             column(6, fileInput('datafile', 'Choose CSV file', accept=c('text/csv','text/comma-separated,text/plain'))),
             br(),
             column(2, downloadButton('downloadData', 'Download')))),
           fluidRow(
             uiOutput('rowcount')
           ),
           hr(),
           fluidRow(
             column(3, 
                    uiOutput('choose_ou'),
                    uiOutput('choose_psnu'),
                    uiOutput('choose_snu_pri'),
                    uiOutput('choose_fa'),
                    uiOutput('choose_im')),
             column(3, 
                    uiOutput('choose_indi'),
                    uiOutput('choose_disagg')),
             column(2, uiOutput('choose_mcad'))
             ),
           hr(),
           fluidRow(DT::dataTableOutput("tab1")))
  )
))
