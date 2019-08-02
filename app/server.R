# SERVER SCRIPT

#1. FETCH DATA from Envir------------------------

# 2. SOURCE all plot scripts-----------------------

# 3. SERVER Starts Here------------------------

server = function(input, output, session) {
  
  #___3.0 SERVER: Refresh Function--------------
  observeEvent(input$refresh1, {
    shinyjs::js$refresh()
  }) 
  
  #___3.1 SERVER: UPDATE Button Logic--------------
  
  rv <- reactiveValues(last_btn = character())
 
  observeEvent(input$update_bttn,{
    if(is.null(input$update_bttn)) return()
    if(input$update_bttn==0) return()
    
    if(rv$last_btn=="Bar"){
      shinyjs::click('Bar')
      
    } else if(rv$last_btn=="Scatter") {
      shinyjs::click('Scatter')
      
    } else if(rv$last_btn=="Histogram") {
      shinyjs::click('Histogram')
      
    } else if(rv$last_btn=="Line") {
      shinyjs::click('Line')
      
    } else if(rv$last_btn=="Box") {
      shinyjs::click('Box')
    }
    
    
  })
  
 
  
  
  #___3.3 SERVER observe the button being pressed--------------
  observeEvent(input$read_dt, {
    if(input$read_dt==TRUE) {
      shinyjs::enable(id = "file1")
      shinyjs::disable(id = "tableName")
      
    } else {
      shinyjs::disable(id = "file1")
      shinyjs::enable(id = "tableName")        
    }
    
  })
  
  #___3.4 SERVER : Reading Data from file--------------
  
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
  
  
  
  #___3.5 SERVER : Update selectInputs--------------
  
  observeEvent(data(), {
    updateSelectInput(session, inputId = "selectX", choices=c(colnames(data()),"None"))
    updateSelectInput(session, inputId = "selectY", choices=c(colnames(data()),"None"))
    
    
    # # update plot parameter dropdowns
    # colorby.choices <- append(colnames(data()),'None')
    # updateSelectInput(session, inputId = "colorby", choices=colorby.choices, selected = 'None')
    
    # update shaby choices scatterplot parameter dropdowns
    # only factor variables will be populated in the dropdown
    shapeby.choices <-  c(names(data())[ sapply(data(), is.factor)],  'None')
    updateSelectInput(session, inputId = "shapeBy", choices=shapeby.choices, selected = 'None')
    
    # update facet row and col selectInputs (only factor vars)
    facet.choices <- c(names(data())[ sapply(data(), is.factor)],  'None')
    updateSelectInput(session, inputId = "selectFacetRow", choices=shapeby.choices, selected = 'None')
    updateSelectInput(session, inputId = "selectFacetCol", choices=shapeby.choices, selected = 'None')
    
  })
  
  #___3.6 SERVER : Displaying Data (sanity check!) ---------
 
  output$tabout <- renderTable({
    
    if(is.null(data())){
      return()
    }
    data()
  })
  
  #___3.7 SERVER : Color By ObserveEvent (to disable colfill) ---------
  
  observeEvent(input$colorby,{
    
    if(input$colorby != 'None'){
      shinyjs::disable('colfill')
    }
    
    if(input$colorby == 'None'){
      shinyjs::enable('colfill')
    }
  })
  
  #___3.8 SERVER : Input Type CHECK ---------
  
  #___3.8.1 INPUT TYPE CHECK: All 3 Conditions Covered ---------
  
  
 
  
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
    
    if (input$Bar > 0 ) {
      rv$last_btn = "Bar"
    }
    
    # update plot parameter dropdowns
    colorby.choices <- append(colnames(data()),'None')
    updateSelectInput(session, inputId = "colorby", choices=colorby.choices, selected = 'None')
    
    
    #______4.0.0 HIDE/SHOW Specific Parameters:------------------------
    
    #________4.0.0.1 hiding scatter specific advance options
    shinyjs::hide("scatter_extra_params")
    shinyjs::hide("lineplot_extra_param")
    shinyjs::hide("dotLine")
    shinyjs::hide("addJitter")
    #________4.0.0.2 Showing Bar specific advance options
    shinyjs::show("barplot_extra_param")
    
    
    
    #______4.0.1 GGPLOT Code--------------------
    
    # shinyjs::toggleElement('barplot_div')
    
    list_both$plot <- 
      bar_plot(data = data(),
                               x=input$selectX,
                               y=input$selectY,
                               plotTitle = input$titleTextBox,
                               Theme = if(input$themeSelect=='None'){"NULL"} else{input$themeSelect},
                               colourfill  = input$colfill,
                               colorby = input$colorby,
                               fontSize = input$axisFont,
                               legendPos = input$legendPosition,
                               title_x = input$titleX,
                               title_y = input$titleY,
                               facetRow = if(input$facetRow != 1){'None'}else{input$selectFacetRow},
                               facetCol = if(input$facetCol != 1){'None'}else{input$selectFacetCol})$plot
    
    
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
               title_y = input$titleY,
               facetRow = if(input$facetRow != 1){'None'}else{input$selectFacetRow},
               facetCol = if(input$facetCol != 1){'None'}else{input$selectFacetCol})$code
    
    
    
    
    
  })
  
  #___4.1 PLOTS CODE: Histogram Plot Code--------------------
  
  observeEvent(input$Histogram,{
    
    if (input$Histogram > 0 ) {
      rv$last_btn = "Histogram"
    }
    
    # update plot parameter dropdowns
    colorby.choices <- append(colnames(data()),'None')
    updateSelectInput(session, inputId = "colorby", choices=colorby.choices, selected = 'None')
    
    #________4.1.0.1 hiding scatter specific advance options
    shinyjs::hide("barplot_extra_param")
    shinyjs::hide("lineplot_extra_param")
    shinyjs::hide("dotLine")
    shinyjs::hide("addJitter")
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
                title_y = input$titleY,
                facetRow = if(input$facetRow != 1){'None'}else{input$selectFacetRow},
                facetCol = if(input$facetCol != 1){'None'}else{input$selectFacetCol})$plot
    
    
    
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
                title_y = input$titleY,
                facetRow = if(input$facetRow != 1){'None'}else{input$selectFacetRow},
                facetCol = if(input$facetCol != 1){'None'}else{input$selectFacetCol})$code
    
    
    
    
    
  })
  
  
  #___4.2 PLOTS CODE: Scatter Plot Code------------------
  
  observeEvent(input$Scatter,{
    
    if (input$Scatter > 0 ) {
      rv$last_btn = "Scatter"
    }
    
    # update plot parameter dropdowns
    colorby.choices <- append(colnames(data()),'None')
    updateSelectInput(session, inputId = "colorby", choices=colorby.choices, selected = 'None')
    
    #________4.2.0.1 hiding scatter specific advance options
    shinyjs::hide("barplot_extra_param")
    shinyjs::hide("lineplot_extra_param")
    shinyjs::hide("dotLine")
    shinyjs::hide("addJitter")
    #________4.2.0.2 Showing scatter specific advance options
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
                   shapeby = input$shapeBy,
                   fontSize = input$axisFont,
                   legendPos = input$legendPosition,
                   dotSize = input$dotSize,
                   dotOpa = input$dotOpa,
                   title_x = input$titleX,
                   title_y = input$titleY,
                   regressionLine = if(input$regLine==FALSE){"NULL"}else{input$regLine},
                   correlation = if(input$corr==FALSE){"NULL"}else{input$corr},
                   facetRow = if(input$facetRow != 1){'None'}else{input$selectFacetRow},
                   facetCol = if(input$facetCol != 1){'None'}else{input$selectFacetCol})$plot
    
    
    #______4.2.1 GGPLOT Code--------------------
    list_both$code <-
      scatter_plot(data = data(),
                   x=input$selectX,
                   y=input$selectY,
                   Theme = if(input$themeSelect=='None'){"NULL"} else{input$themeSelect},
                   plotTitle = input$titleTextBox,
                   colourfill = input$colfill,
                   colorby = input$colorby,
                   shapeby = input$shapeBy,
                   fontSize = input$axisFont,
                   legendPos = input$legendPosition,
                   dotSize = input$dotSize,
                   dotOpa = input$dotOpa,
                   title_x = input$titleX,
                   title_y = input$titleY,
                   regressionLine = if(input$regLine==FALSE){"NULL"}else{input$regLine},
                   correlation = if(input$corr==FALSE){"NULL"}else{input$corr},
                   facetRow = if(input$facetRow != 1){'None'}else{input$selectFacetRow},
                   facetCol = if(input$facetCol != 1){'None'}else{input$selectFacetCol})$code
    
    
    
    
  })
  
  #___4.3 PLOTS CODE: Line Plot Code------------------
  
  observeEvent(input$Line,{
    
    if (input$Line > 0 ) {
      rv$last_btn = "Line"
    }
    
    # update plot parameter dropdowns
    colorby.choices <- append(colnames(data()),'None')
    updateSelectInput(session, inputId = "colorby", choices=colorby.choices, selected = 'None')
    
    #________4.3.0.1 hiding Line specific advance options
    shinyjs::hide("addJitter")
    shinyjs::hide("barplot_extra_param")
    shinyjs::hide("scatter_extra_params")
    #________4.3.0.2 Show Line specific advance options
    shinyjs::show("lineplot_extra_param")
    shinyjs::show("dotLine")
    
    #______4.3.0 Plot Code--------------------
    list_both$plot <-
      line_plot(data = data(),
                x=input$selectX,
                y=input$selectY,
                Theme = if(input$themeSelect=='None'){"NULL"} else{input$themeSelect},
                plotTitle = input$titleTextBox,
                colourfill = input$colfill,
                colorby = input$colorby,
                fontSize = input$axisFont,
                legendPos = input$legendPosition,
                title_x = input$titleX,
                title_y = input$titleY,
                lineType = input$lineplot_extra_param,
                dots = input$dotLine,
                facetRow = if(input$facetRow != 1){'None'}else{input$selectFacetRow},
                facetCol = if(input$facetCol != 1){'None'}else{input$selectFacetCol})$plot
    
    
    #______4.3.1 GGPLOT Code--------------------
    list_both$code <- 
      line_plot(data = data(),
                x=input$selectX,
                y=input$selectY,
                Theme = if(input$themeSelect=='None'){"NULL"} else{input$themeSelect},
                plotTitle = input$titleTextBox,
                colourfill = input$colfill,
                colorby = input$colorby,
                fontSize = input$axisFont,
                legendPos = input$legendPosition,
                title_x = input$titleX,
                title_y = input$titleY,
                lineType = input$lineplot_extra_param,
                dots = input$dotLine,
                facetRow = if(input$facetRow != 1){'None'}else{input$selectFacetRow},
                facetCol = if(input$facetCol != 1){'None'}else{input$selectFacetCol})$code
    
  })
  
  #___4.4 PLOTS CODE: Box Plot Code------------------
  
  observeEvent(input$Box,{
    
    if (input$Box > 0 ) {
      rv$last_btn = "Box"
    }
    
   
    # update plot parameter dropdowns
    colorby.choices <- c(names(data())[ sapply(data(), is.factor)],  'None')
    updateSelectInput(session, inputId = "colorby", choices=colorby.choices, selected = 'None')
    
    
    #________4.4.0.1 hiding specific advance options
    shinyjs::hide("barplot_extra_param")
    shinyjs::hide("lineplot_extra_param")
    shinyjs::hide("dotLine")
    shinyjs::hide("scatter_extra_params")
    #________4.4.0.2 Showing Box specific advance options
    shinyjs::show("addJitter")
    
    #______4.4.0 Plot Code--------------------
    list_both$plot <-
      box_plot(data = data(),
                x=input$selectX,
                y=input$selectY,
                 plotTitle = input$titleTextBox,
                 Theme = if(input$themeSelect=='None'){"NULL"} else{input$themeSelect},
                 colourfill  = input$colfill,
                 colorby = input$colorby,
                 fontSize = input$axisFont,
                 legendPos = input$legendPosition,
                 title_x = input$titleX,
                 title_y = input$titleY,
                 jitter = input$addJitter,
                 facetRow = if(input$facetRow != 1){'None'}else{input$selectFacetRow},
                 facetCol = if(input$facetCol != 1){'None'}else{input$selectFacetCol})$plot
    
    
    #______4.4.1 GGPLOT Code--------------------
    list_both$code <- 
      box_plot(data = data(),
                x=input$selectX,
                y=input$selectY,
                 plotTitle = input$titleTextBox,
                 Theme = if(input$themeSelect=='None'){"NULL"} else{input$themeSelect},
                 colourfill  = input$colfill,
                 colorby = input$colorby,
                 fontSize = input$axisFont,
                 legendPos = input$legendPosition,
                 title_x = input$titleX,
                 title_y = input$titleY,
                 jitter = input$addJitter,
                 facetRow = if(input$facetRow != 1){'None'}else{input$selectFacetRow},
                 facetCol = if(input$facetCol != 1){'None'}else{input$selectFacetCol})$code
    
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

