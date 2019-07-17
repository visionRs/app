#1. Importing all useful libraries------------------------

library(shiny)
library(shinydashboard)
library(shinyFiles)
library(ggplot2)
library(shinycssloaders)
library(shinyWidgets)
library(R.utils)


#2. Sourcing all useful scripts------------------------
source('00_Header.R', echo=F)
source('01_Sidebar.R', echo=F)
source('02_Body.R', echo=F)
sourceDirectory('plots/')



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
        get(input$tableName)
        
        
      }
    })
    
    
    
    #___4.3 SERVER : Update selectInputs--------------
    
    observeEvent(data(), {
      updateSelectInput(session, inputId = "selectX", choices=colnames(data()))
      updateSelectInput(session, inputId = "selectY", choices=colnames(data()))
    })
    
    #___4.4 SERVER : Displaying Data (sanity check!) ---------
    output$tabout <- renderTable({
      
      if(is.null(data())){
        return()
      }
      data()
    })
    
    
    
    #5 PLOTS CODE: -------------
    output$basic_barplot <- renderPlot({
      dt <- data()
      if(is.null(dt)){return()}
      if(is.null(input$radioPlot)){return()}
      
      switch(input$radioPlot,
             "Bar" =    bar_plot(data = dt,x=input$selectX,y=input$selectY)$plot,
             "Scatter" = scatter_plot(data = dt,x=input$selectX,y=input$selectY)$plot,
             "Line" =    line_plot(data = dt,x=input$selectX,y=input$selectY)$plot
      )
      
      
      
      
      
    })
    
    
    #5 RETURN CODE BLOCK: -------------
    output$return_code <- renderText({
      dt <- data()
      if(is.null(dt)){return()}
      if(is.null(input$radioPlot)){return()}
      
      switch(input$radioPlot,
             "Bar" =    bar_plot(data = dt,x=input$selectX,y=input$selectY)$code,
             "Scatter" = scatter_plot(data = dt,x=input$selectX,y=input$selectY)$code,
             "Line" =    line_plot(data = dt,x=input$selectX,y=input$selectY)$code
      )
      
      
      
      
      
    })
    
  }
  
)