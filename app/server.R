# SERVER SCRIPT

#1. FETCH DATA from Envir------------------------

# 2. SOURCE all plot scripts-----------------------

# 3. SERVER Starts Here------------------------

server = function(input, output, session) {
  
  #___3.1 SERVER: Refresh Function--------------
  observeEvent(input$refresh, {
    shinyjs::js$refresh()
  }) 
  
  
  ## observe the button being pressed
  observeEvent(input$read_dt, {
    if(input$read_dt==TRUE) {
      shinyjs::enable(id = "file1")
      shinyjs::disable(id = "tableName")
      
    } else {
      shinyjs::disable(id = "file1")
      shinyjs::enable(id = "tableName")        
    }
    
  })
  
  #___3.2 SERVER : Reading Data from file--------------
  
  data <-  reactive({
    if(!is.null(input$file1)){
      inFile <- input$file1
      read.csv(inFile$datapath) 
      
    } else {
      if(input$tableName!="None"){
        get(input$tableName)
      }
      
    }
  })
  
  
  
  #___3.3 SERVER : Update selectInputs--------------
  
  observeEvent(data(), {
    updateSelectInput(session, inputId = "selectX", choices=c(colnames(data()),"None"))
    updateSelectInput(session, inputId = "selectY", choices=c(colnames(data()),"None"))
    
    # update plot parameter dropdowns
    colorby.choices <- append(colnames(data()),'None')
    updateSelectInput(session, inputId = "colorby", choices=colorby.choices, selected = 'None')
    print(input$colorby == 'None')
    
  })
  
  #___3.4 SERVER : Displaying Data (sanity check!) ---------
 
  output$tabout <- renderTable({
    
    if(is.null(data())){
      return()
    }
    data()
  })
  
  #___3.5 SERVER : Input Type CHECK ---------
  
  #___3.5.1 INPUT TYPE CHECK: All 3 Conditions Covered ---------
  
  
  observeEvent(input$colorby,{
    
    if(input$colorby != 'None'){
      shinyjs::disable('colfill')
    }
    
    if(input$colorby == 'None'){
      shinyjs::enable('colfill')
    }
  })
  
  observeEvent(c(input$selectX,input$selectY), {
    dt <- data()
   
    if((!is.numeric(dt[[input$selectX]]) & !is.numeric(dt[[input$selectY]])) | (is.null(dt[[input$selectX]]) & is.null(dt[[input$selectY]])) | is.null(dt[[input$selectX]])){
      shinyjs::disable("Bar")
      shinyjs::disable("Histogram")
      shinyjs::disable("Scatter")
      shinyjs::disable("Line")
      shinyjs::disable("Box")
      
      
    } else if((is.null(dt[[input$selectY]]) & is.numeric(dt[[input$selectX]]))) {
      
      shinyjs::disable("Bar")
      shinyjs::disable("Scatter")
      shinyjs::disable("Line")
      shinyjs::disable("Box")
      shinyjs::enable("Histogram")
      
      
    } else if(!is.numeric(dt[[input$selectX]]) | !is.numeric(dt[[input$selectY]])) {
      
      shinyjs::enable("Bar")
      shinyjs::disable("Scatter")
      shinyjs::enable("Line")
      shinyjs::disable("Histogram")
      shinyjs::enable("Box")
      
      
    } else {
      
      shinyjs::enable("Bar")
      shinyjs::enable("Scatter")
      shinyjs::enable("Line")
      shinyjs::enable("Box")
      shinyjs::disable("Histogram")
      
    }
    
    # update textInputs for renaming axes
    updateTextInput(session = session,inputId = "titleX", value = input$selectX)
    updateTextInput(session = session,inputId = "titleY", value = input$selectY)
  })
  
  #4 PLOTS CODE: -------------
  
  list_both <- reactiveValues(plot = NULL,
                              code = NULL)
  
  
  #___4.0 PLOTS CODE: Bar Plot Code-----------------
  observeEvent(input$Bar,{
    
    #______4.0.0 HIDE/SHOW Specific Parameters:------------------------
    
    #________4.0.0.1 hiding scatter specific advance options
    shinyjs::hide("scatter_extra_params")
    shinyjs::hide("lineplot_extra_param")
    #________4.0.0.2 Showing Bar specific advance options
    shinyjs::show("barplot_extra_param")
    
    
    
    #______4.0.1 GGPLOT Code--------------------
    
    # shinyjs::toggleElement('barplot_div')
    
    list_both$plot <- bar_plot(data = data(),
                               x=input$selectX,
                               y=input$selectY,
                               plotTitle = input$titleTextBox,
                               Theme = if(input$themeSelect=='None'){"NULL"} else{input$themeSelect},
                               colourfill  = input$colfill,
                               colorby = input$colorby,
                               fontSize = input$axisFont,
                               legendPos = input$legendPosition,
                               title_x = input$titleX,
                               title_y = input$titleY)$plot
    
    
    #______4.0.2 GGPLOT Code--------------------
    list_both$code <-
      bar_plot(data = data(),
               x=input$selectX,
               y=input$selectY,
               plotTitle = input$titleTextBox,
               Theme = if(input$themeSelect=='None'){"NULL"} else{input$themeSelect},
               colourfill = input$colfill,
               colorby = input$colorby,
               fontSize = input$axisFont,
               legendPos = input$legendPosition,
               title_x = input$titleX,
               title_y = input$titleY)$code
    
    
    
    
    
  })
  
  #___4.1 PLOTS CODE: Histogram Plot Code--------------------
  
  observeEvent(input$Histogram,{
    # hiding Bar specific advance options
    shinyjs::hide("barplot_extra_param")
    shinyjs::hide("lineplot_extra_param")
    # hiding Scatter specific advance options
    shinyjs::hide("scatter_extra_params")
    
    #______4.1.0 Plot Code--------------------
    
    list_both$plot <-
      histogram(data = data(),
                x=input$selectX,
                Theme = if(input$themeSelect=='None'){"NULL"} else{input$themeSelect},
                plotTitle = input$titleTextBox,
                fontSize = input$axisFont,
                colourfill = input$colfill,
                colorby = input$colorby,
                legendPos = input$legendPosition,
                title_x = input$titleX,
                title_y = input$titleY)$plot
    
    
    
    #______4.1.1 GGPLOT Code--------------------
    
    list_both$code <-
      histogram(data = data(),
                x=input$selectX,
                Theme = if(input$themeSelect=='None'){"NULL"} else{input$themeSelect},
                plotTitle = input$titleTextBox,
                fontSize = input$axisFont,
                colourfill = input$colfill,
                colorby = input$colorby,
                legendPos = input$legendPosition,
                title_x = input$titleX,
                title_y = input$titleY)$code
    
    
    
    
    
  })
  
  
  #___4.2 PLOTS CODE: Scatter Plot Code------------------
  
  observeEvent(input$Scatter,{
    
    # hiding Bar specific advance options
    shinyjs::hide("barplot_extra_param")
    shinyjs::hide("lineplot_extra_param")
    # Showing Scatter specific advance options
    shinyjs::show("scatter_extra_params")
    
    #______4.2.0 Plot Code--------------------
    list_both$plot <-
      scatter_plot(data = data(),
                   x=input$selectX,
                   y=input$selectY,
                   Theme = if(input$themeSelect=='None'){"NULL"} else{input$themeSelect},
                   plotTitle = input$titleTextBox,
                   colourfill = input$colfill,
                   colorby = input$colorby,
                   fontSize = input$axisFont,
                   legendPos = input$legendPosition,
                   dotSize = input$dotSize,
                   dotOpa = input$dotOpa,
                   title_x = input$titleX,
                   title_y = input$titleY,
                   regressionLine = if(input$regLine==FALSE){"NULL"}else{input$regLine},
                   correlation = if(input$corr==FALSE){"NULL"}else{input$corr})$plot
    
    
    #______4.2.1 GGPLOT Code--------------------
    list_both$code <-
      scatter_plot(data = data(),
                   x=input$selectX,
                   y=input$selectY,
                   Theme = if(input$themeSelect=='None'){"NULL"} else{input$themeSelect},
                   plotTitle = input$titleTextBox,
                   colourfill = input$colfill,
                   colorby = input$colorby,
                   fontSize = input$axisFont,
                   legendPos = input$legendPosition,
                   dotSize = input$dotSize,
                   dotOpa = input$dotOpa,
                   title_x = input$titleX,
                   title_y = input$titleY,
                   regressionLine = if(input$regLine==FALSE){"NULL"}else{input$regLine},
                   correlation = if(input$corr==FALSE){"NULL"}else{input$corr})$code
    
    
    
    
  })
  
  #___4.3 PLOTS CODE: Line Plot Code------------------
  
  observeEvent(input$Line,{
    shinyjs::show("lineplot_extra_param")
    # hiding Bar specific advance options
    shinyjs::hide("barplot_extra_param")
    # hiding Scatter specific advance options
    shinyjs::hide("scatter_extra_params")
    
    #______4.3.0 Plot Code--------------------
    list_both$plot <-
      line_plot(data = data(),
                x=input$selectX,
                y=input$selectY,
                Theme = input$themeSelect,
                plotTitle = input$titleTextBox,
                colourfill = input$colfill,
                colorby = input$colorby,
                fontSize = input$axisFont,
                legendPos = input$legendPosition,
                title_x = input$titleX,
                title_y = input$titleY)$plot
    
    
    #______4.3.1 GGPLOT Code--------------------
    list_both$code <- 
      line_plot(data = data(),
                x=input$selectX,
                y=input$selectY,
                Theme = input$themeSelect,
                plotTitle = input$titleTextBox,
                colourfill = input$colfill,
                colorby = input$colorby,
                fontSize = input$axisFont,
                legendPos = input$legendPosition,
                title_x = input$titleX,
                title_y = input$titleY)$code
    
  })
  
  #___4.4 PLOTS CODE: Box Plot Code------------------
  
  observeEvent(input$Box,{
    
    # hiding Bar specific advance options
    shinyjs::hide("barplot_extra_param")
    shinyjs::hide("lineplot_extra_param")
    # hiding Scatter specific advance options
    shinyjs::hide("scatter_extra_params")
    
    #______4.4.0 Plot Code--------------------
    list_both$plot <-
      box_plot(data = data(),
                x=input$selectX,
                y=input$selectY)$plot
    
    
    #______4.4.1 GGPLOT Code--------------------
    list_both$code <- 
      box_plot(data = data(),
                x=input$selectX,
                y=input$selectY)$code
    
  })
  
  
  #5. Final RenderPlot Code for GGPLOT----------------------
  output$plot <- renderPlot({
    if (is.null(list_both$plot)) return()
    isolate({
      list_both$plot
    })
  })
  
  #6. Final RenderText Code for GGPLOT----------------------
  output$code <- renderText({
    
    if (is.null(list_both$code)) return()
    isolate({
      
      list_both$code
    })
  })
  
  
  
} # server ends here

