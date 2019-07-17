#1. Sourcing all useful scripts------------------------
source('~/working_dir/projects/Easy-Plot/00_Header.R', echo=F)
source('~/working_dir/projects/Easy-Plot/01_Sidebar.R', echo=F)
source('~/working_dir/projects/Easy-Plot/02_Body.R', echo=F)
source('~/working_dir/projects/Easy-Plot/03_server.R', echo=F)
source('~/working_dir/projects/Easy-Plot/plots/00_Plots-R-Code.R', echo=F)

#2. Importing all useful libraries------------------------

library(shiny)
library(shinydashboard)
library(shinyFiles)
library(ggplot2)
library(shinycssloaders)
library(shinyWidgets)


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
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath) 
    
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
           "Bar" =    bar_plot(data = dt,x=input$selectX,y=input$selectY),
           "Scatter" = scatter_plot(data = dt,x=input$selectX,y=input$selectY),
           "Line" =    line_plot(data = dt,x=input$selectX,y=input$selectY)
    )
    
    
    
    
    
  })
}

)