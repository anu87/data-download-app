

library(shiny)
library(tidyverse)
library(DT)
# library(readr)
library(data.table)
library(shinyWidgets) # for dropdown menu

shinyServer(function(input, output, session) {
  options(shiny.maxRequestSize=10000*1024^2) # set the max file size that can uploaded

# upload fn
  dfip3<- reactive({
    if(is.null(input$datafile))
      return()
    input$datafile
    # dfip1<-read.delim(input$datafile$datapath, header= T, stringsAsFactors = F, fileEncoding="UTF-8-BOM") # reading string as factors - works for the menu variables we need
    # dfip1<- read_delim(input$datafile$datapath, col_names = T, delim = '\t', col_types = cols(.default='c'))
    dfip1<- fread(input$datafile$datapath,header = T, stringsAsFactors = T)
    dfip1
  })
  
# Read & Display columns names
  output$choose_col <- renderUI({
    if(is.null(input$datafile))
      return()
    df1<-dfip3()
    checkboxGroupInput('show_vars', "Select Columns", names(df1), selected = names(df1), width = 500)
   })
 
# Select all / Unselect all for Columns
observeEvent(input$selectall,{
  if(is.null(input$show_vars)){
    updateCheckboxGroupInput(session = session, inputId = "show_vars", choices=names(dfip3()), selected = names(dfip3()))
  }
  else {updateCheckboxGroupInput(session = session, inputId = "show_vars", choices=names(dfip3()), selected = "")}
})

# Replace na in PSNU with 'Blank'
dfip2<- reactive({
  # if(is.null(input$datafile))
  #   {return()}
  # else{
    if('PSNU' %in% colnames(dfip3())){ # checking if PSNU exists in the dataset
      p<- dfip3()
      levels(p$PSNU)[levels(p$PSNU)==""] <- "Blank"
      p$PSNU<-gsub('_', '', paste(p$PSNU))
      return(p)}
    else{return()}
  # }
})

# Replace na in SNU_Prioritization
dfip4<- reactive({
  if(is.null(input$datafile))
    return()
  j<- dfip2()
  # levels(j$FY16SNUPrioritization)[levels(j$FY16SNUPrioritization)==""] <- "Blank"
  j[is.na(j)]<- "Blank"
  return(j)
})


# Paste space in front of age to stop DT & excel from reaing it as date
  dfip<- reactive({
    if(is.null(input$datafile))
      return()
    c1<-dfip4()
    c1$Age<-paste(" ", c1$Age)
    return(c1)
  })

# OU drop-down menu
  output$choose_ou <- renderUI({ # create reactive ui element inside server, which will called in ui
    if(is.null(input$datafile)) # necessary to avoid NULL error before data is uploaded
      return()
    ou_UP<-sort(as.vector(unique(dfip()$OperatingUnit))) # create a list of unique Operating units
    selectInput('check1', "choosen ou", ou_UP) # initiate selectInput for dropdown
    dropdownButton(
      label = "Select OU", status = "default", width = 200, circle = F,
      tags$div(
        class='container',
        actionButton(inputId = "all1", label = "Select all"), 
        checkboxGroupInput(inputId = "check1", label = "Choose", paste(ou_UP))
      )
    )
  })
  
  
  # Select all / Unselect all for OU 
  observeEvent(input$all1, {
    if (is.null(input$check1)) {
      ou_UP<-sort(as.vector(unique(dfip()$OperatingUnit)))
      updateCheckboxGroupInput(
        session = session, inputId = "check1", selected = paste(ou_UP)
      )} else {
        updateCheckboxGroupInput(
          session = session, inputId = "check1", selected = "")}
  })
  
 
# Indicator Drop-down menu
  dfindi<- reactive({
    if(is.null(input$datafile))
      return()
    v<-dfip()
    levels(v$indicator)[levels(v$indicator)=='']<-'Blank'
    return(v)
  })
  output$choose_indi <- renderUI({
    if(is.null(input$datafile))
      return()
    indi_UP<-sort(as.vector(unique(dfindi()$indicator)))
    selectInput('check2', "choosen indi", indi_UP)
    dropdownButton(
      label = "Select Indicator(s)", status = "default", width = 400, circle = F,
      tags$div(
        class='container',
        actionButton(inputId = "all2", label = "Select all"),
        checkboxGroupInput(inputId = "check2", label = "Choose", paste(indi_UP))
      ))
  })
  
  # Select all / Unselect all for Indicator
  observeEvent(input$all2, {
    if (is.null(input$check2)) {
      indi_UP<-sort(as.vector(unique((dfindi()$indicator))))
      # indi_UP<-sort(indi_UP)
      updateCheckboxGroupInput(
        session = session, inputId = "check2", selected = paste(indi_UP)
      )} else {updateCheckboxGroupInput(session = session, inputId = "check2", selected = "")}
  }) 
  
  # MCAD Drop-down menu
  output$choose_mcad <- renderUI({
    if(is.null(input$datafile))
      return()
    df9<-dfip()
    # levels(df9$isMCAD)[levels(df9$isMCAD)=='']<-'Blank' # this cmd doesn't work with dfip() directly - so need to use df9
    mcad<-as.list(unique(as.character(df9$isMCAD)))
    selectInput('check9', "choosen mcad", mcad)
    dropdownButton(
      label = "Select MCAD", status = "default", width = 150, circle = F,
      # actionButton(inputId = "all9", label = "Select all"),
      checkboxGroupInput(inputId = "check9", label = "Choose", paste(mcad), selected = paste(mcad))
    )
  })
 
# PSNU drop-down menu 
  d1<- reactive({
    d1 = filter(dfip(), OperatingUnit %in% input$check1)
    return(d1)
  })
  output$choose_psnu <- renderUI({
    if(is.null(input$check1))
      return()
    psnu <- sort(as.vector(unique(d1()$PSNU)))
    dropdownButton(
      label = "Select PSNU", status = "default", width = 150, circle = F,
      tags$div(
        class='container',
        actionButton(inputId = "all3", label = "Select all"),
        checkboxGroupInput(inputId = "check3", label = "Choose", paste(psnu), selected = paste(psnu)
    )))
  })

## PSNU select-all  
  observeEvent(input$all3, {
    if (is.null(input$check3)) {
      psnu <- sort(as.vector(unique(d1()$PSNU)))
      updateCheckboxGroupInput(
        session = session, inputId = "check3", selected = paste(psnu)
      )} else {
      updateCheckboxGroupInput(
        session = session, inputId = "check3", selected = "")}
  })

## FY16-SNU Prioritization Dropdown menu
  d2<- reactive({
    d2 = filter(dfip(), PSNU %in% input$check3)
    return(d2)
  })
  
  output$choose_snu_pri <- renderUI({
    if(is.null(input$check3))
      return()
    snuP <- as.list(unique(as.character(d2()$FY16SNUPrioritization)))
    dropdownButton(
      label = "Select FY16SNU_Prioritization", status = "default", width = 150, circle = F,
      actionButton(inputId = "all4", label = "Select all"),
      checkboxGroupInput(inputId = "check4", label = "Choose", paste(snuP), selected = paste(snuP))
    )
  })

##   FY16-SNU Prioritization select-all
  observeEvent(input$all4, {
    if (is.null(input$check4)) {
      snuP <- as.list(unique(as.character(d2()$FY16SNUPrioritization)))
      updateCheckboxGroupInput(
        session = session, inputId = "check4", selected = paste(snuP)
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "check4", selected = ""
      )
    }
  })

## Funding Agency drop-down menu 
  dfa<-reactive({
    dfa = filter(dfip(), PSNU %in% input$check3)
  })
  
  output$choose_fa <- renderUI({
    if(is.null(input$check3))
      return()
    fa <- sort(as.vector(unique(dfa()$FundingAgency)))
    dropdownButton(
      label = "Select Funding Agency", status = "default", width = 150, circle = F,
      tags$div(
        class='container',
        actionButton(inputId = "all5", label = "Select all"),
        checkboxGroupInput(inputId = "check5", label = "Choose", paste(fa), selected = paste(fa))
    ))
  })
  
## FUNDING AGENCY select - all  
  observeEvent(input$all5, {
    if (is.null(input$check5)) {
      fa <- sort(as.vector(unique(dfa()$FundingAgency)))
      updateCheckboxGroupInput(
        session = session, inputId = "check5", selected = paste(fa)
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "check5", selected = ""
      )
    }
  })

## IMPLEMENTATION MECHANISM drop-down menu  
  dfim<- reactive({
    dfim = filter(dfip(), FundingAgency %in% input$check5)
    return(dfim)
  })
  
  output$choose_im <- renderUI({
    if(is.null(input$check5))
      return()
    im <- sort(as.vector(unique(dfim()$ImplementingMechanismName)))
    dropdownButton(
      label = "Select Implementation Mechanism", status = "default", width = 250, circle = F,
      tags$div(
        class='container',
        actionButton(inputId = "all6", label = "Select all"),
        checkboxGroupInput(inputId = "check6", label = "Choose", paste(im), selected = paste(im))
    ))
  })

## IMPLEMENTATION MECHANISM select-all
  observeEvent(input$all6, {
    if (is.null(input$check6)) {
      im <- sort(as.vector(unique(dfim()$ImplementingMechanismName)))
      updateCheckboxGroupInput(
        session = session, inputId = "check6", selected = paste(im)
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "check6", selected = ""
      )
    }
  })

# Standardized Disaggregate drop-down menu
  
  dfdis<- reactive({
    dfdis = filter(dfip(), indicator %in% input$check2)
    return(dfdis)
  })
  
  output$choose_disagg <- renderUI({
    if(is.null(input$check2))
      return()
    # levels(dfdis$standardizedDisaggregate)[levels(dfdis$standardizedDisaggregate)==""]<- Blank
    dis <- sort(as.vector(unique(dfdis()$standardizedDisaggregate)))
    dropdownButton(
      label = "Select Standardized Disagg", status = "default", width = 300, circle = F,
      tags$div(
        class='container',
        actionButton(inputId = "all7", label = "Select all"),
        checkboxGroupInput(inputId = "check7", label = "Choose", paste(dis), selected = paste(dis))
    ))
  })
  
  observeEvent(input$all7, {
    if (is.null(input$check7)) {
      dis <- sort(as.vector(unique(dfdis()$standardizedDisaggregate)))
      updateCheckboxGroupInput(
        session = session, inputId = "check7", selected = paste(dis)
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "check7", selected = ""
      )
    }
  })
 
# subset data based on selections in the drop down menu  
  df2<- reactive({
    if(is.null(input$datafile))
      return()
    a2<- dfip()
    return(filter(a2, OperatingUnit %in% input$check1, indicator %in% input$check2, standardizedDisaggregate %in% input$check7
                  , PSNU %in% input$check3
                  , FY16SNUPrioritization %in% input$check4
                  , FundingAgency %in% input$check5
                  , ImplementingMechanismName %in% input$check6
                  , isMCAD %in% input$check9
                  ))
    
  })

#subset data based on selection of columns
  df3<- reactive({
    d <- df2()[, input$show_vars] # show_vars is input id of column checkbox
    return(d)
    })

# Display data table based on selections in dropdown menu & columns
  output$tab1<- renderDataTable({
    df2()[, input$show_vars, drop=FALSE]
  })

  output$rowcount<- renderText({
    paste("Number of rows:",format(nrow(df2()), big.mark=",", scientific=FALSE))
  })
    
# Choose df3() and download the file  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("PSNU_IM", '.csv', sep='')
    },
    content = function(file) {
      write.csv(df3(), file, row.names = F)
    }
  )
})
