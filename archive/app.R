#1. Importing all useful libraries------------------------

library(shiny)
library(shinydashboard)
library(shinyFiles)
library(ggplot2)
library(shinycssloaders)
library(shinyWidgets)
library(R.utils)
 


#2. Sourcing all useful scripts------------------------
source('00_Header.R')
source('01_Sidebar.R')
source('02_Body.R')

# List dfs


#Source all files in a directory
sapply(list.files("plots",full.names = T),source)

# List dfs



#3. Calling all functions from sourced scripts------------------------

shinyApp(
  
  ui = dashboardPage(
    header,
    sidebar,
    body
  ),
  
  
  #4. Server Starts Here------------------------
  
  server = function(input, output, session) {
    
    #___4.1 SERVER: Refresh Function
    observeEvent(input$refresh1, {
      shinyjs::js$refresh()
    }) 
    
    
    #___4.2 SERVER : Reading Data from file--------------
    
    data <-  reactive({
      if(!is.null(input$file1)){
        inFile <- input$file1
        read.csv(inFile$datapath) 
        
      } else {
        if(input$tableName!="None"){
        get(input$tableName, envir = .GlobalEnv)
        }
        
      }
    })
    
    
    
    #___4.3 SERVER : Update selectInputs--------------
    
    observeEvent(data(), {
      updateSelectInput(session, inputId = "selectX", choices=colnames(data()))
      updateSelectInput(session, inputId = "selectY", choices=colnames(data()))
      
      # update plot parameter dropdowns
      colorby.choices <- append(colnames(data()),'None')
      updateSelectInput(session, inputId = "colorby", choices=colorby.choices, selected = 'None')
    })
    
    #___4.4 SERVER : Displaying Data (sanity check!) ---------
    output$tabout <- renderTable({
      
      if(is.null(data())){
        return()
      }
      data()
    })
    
    #___4.5 SERVER : Input Type CHECK ---------
    
    #___4.5.1 INPUT TYPE CHECK: All 3 Conditions Covered ---------
    
    observeEvent(c(input$selectX,input$selectY), {
      dt <- data()
      if(!is.numeric(dt[[input$selectX]]) & !is.numeric(dt[[input$selectY]])){
        updateRadioGroupButtons(session = session,inputId = "radioPlot",
                                choices = c(""),
                                checkIcon = list(yes = icon("ok", 
                                                            lib = "glyphicon")),
                                selected = F,
                                status = "warning")
      } else if(!is.numeric(dt[[input$selectX]]) | !is.numeric(dt[[input$selectY]])) {
        
        updateRadioGroupButtons(session = session,inputId = "radioPlot",
                                choices = c("Bar", "Line"),
                                checkIcon = list(yes = icon("ok", 
                                                            lib = "glyphicon")),
                                selected = F,
                                status = "warning")              
       } else {
         
         updateRadioGroupButtons(session = session,inputId = "radioPlot",
                                 choices = c("Bar", "Scatter","Line"),
                                 checkIcon = list(yes = icon("ok", 
                                                             lib = "glyphicon")),
                                 selected = F,
                                 status = "warning") 
       }
    })
    
    #5 PLOTS CODE: -------------
    output$basic_barplot <- renderPlot({
      dt <- data()
      if(is.null(dt)){return()}
      if(is.null(input$radioPlot)){return()}
      
      switch(input$radioPlot,
             "Bar" =    bar_plot(data = dt,x=input$selectX,y=input$selectY, colorby = input$colorby, 
                                 fontSize = input$axisFont, legendPos = input$legendPosition)$plot,
             "Scatter" = scatter_plot(data = dt,x=input$selectX,y=input$selectY, colorby = input$colorby, 
                                      fontSize = input$axisFont, legendPos = input$legendPosition,
                                      dotSize = input$dotSize)$plot,
             "Line" =    line_plot(data = dt,x=input$selectX,y=input$selectY, colorby = input$colorby, 
                                   fontSize = input$axisFont, legendPos = input$legendPosition)$plot
      )
     
    })
    
    
    #5 RETURN CODE BLOCK: -------------
    output$return_code <- renderText({
      dt <- data()
      if(is.null(dt)){return()}
      if(is.null(input$radioPlot)){return()}
      
      switch(input$radioPlot,
             "Bar" =    bar_plot(data = dt,x=input$selectX,y=input$selectY, colorby = input$colorby, 
                                 fontSize = input$axisFont, legendPos = input$legendPosition)$code,
             "Scatter" = scatter_plot(data = dt,x=input$selectX,y=input$selectY, colorby = input$colorby, 
                                      fontSize = input$axisFont, legendPos = input$legendPosition,
                                      dotSize = input$dotSize)$code,
             "Line" =    line_plot(data = dt,x=input$selectX,y=input$selectY, colorby = input$colorby, 
                                   fontSize = input$axisFont, legendPos = input$legendPosition)$code
      )
       
    })
    
  }
  
)