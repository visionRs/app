server_code = function(input, output, session) {
  
  
  #Refresh Function
  observeEvent(input$refresh1, {
    shinyjs::js$refresh()
  }) 
  
  
  data <-  reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    # read table
    read.csv(inFile$datapath) 
    
  })
  
  # to update selectInputs
  observeEvent(data(), {
    updateSelectInput(session, inputId = "selectX", choices=colnames(data()))
    updateSelectInput(session, inputId = "selectY", choices=colnames(data()))
  })
  
  # showing table (sanity check!)
  output$tabout <- renderTable({
    
    if(is.null(data())){
      return()
    }
    data()
  })
  
  
  
  # PLOTS CODE: 
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