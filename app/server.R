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
    
    factor_vars <-  c(names(data())[ sapply(data(), is.factor)])
    char_vars <-  c(names(data())[ sapply(data(), is.character)])
    numeric_vars <-  c(names(data())[ sapply(data(), is.numeric)])
    int_vars <-  c(names(data())[ sapply(data(), is.integer)])
    
    showModal(modalDialog(
      title = h4(tags$b("Data Summary")),
      h5(tags$b("Data Dimensions")),
      p(paste0("Rows: ", nrow(data()))),
      p(paste0("Columns: ", ncol(data()))),
      br(),
      h5(tags$b("Variable class")),
      p(ifelse(is_empty(factor_vars), paste0("Factor variables: None"), paste0("Factor variables: ", paste(factor_vars, collapse = ", ")))),
      p(ifelse(is_empty(char_vars), paste0("Character variables: None"), paste0("Character variables: ", paste(char_vars, collapse = ", ")))),
      p(ifelse(is_empty(numeric_vars), paste0("Numeric variables: None"), paste0("Numeric variables: ", paste(numeric_vars, collapse = ", ")))),
      p(ifelse(is_empty(int_vars), paste0("Integer variables: None"), paste0("Integer variables: ", paste(int_vars, collapse = ", ")))),
      easyClose = TRUE
    ))
    updateSelectInput(session, inputId = "selectX", choices=c(colnames(data()),"None"))
    updateSelectInput(session, inputId = "selectY", choices=c(colnames(data()),"None"))
    
    
    # # update plot parameter dropdowns
    # colorby.choices <- append(colnames(data()),'None')
    # updateSelectInput(session, inputId = "colorby", choices=colorby.choices, selected = 'None')
    
    # update shaby choices scatterplot parameter dropdowns
    # only factor variables will be populated in the dropdown
    shapeby.choices <-  c(names(data())[ sapply(data(), is.factor)],  'None')
    updateSelectInput(session, inputId = "shapeBy", choices=shapeby.choices, selected = 'None')
    
    # # update facet row and col selectInputs (only factor vars)
    # facet.choices <- c(names(data())[ sapply(data(), is.factor)],  'None')
    # updateSelectInput(session, inputId = "selectFacetRow", choices=facet.choices, selected = 'None')
    # updateSelectInput(session, inputId = "selectFacetCol", choices=facet.choices, selected = 'None')
    # 
  })
 
  
  #_____3.5.1 SERVER : update facet row and col selectInputs (only factor vars) ----
  observeEvent(c(input$facetRow, input$facetCol), {
    facet.choices <- c(names(data())[ sapply(data(), is.factor)],  'None')
    shinyjs::enable('facetRow')
    shinyjs::enable('facetCol')
    
   if(length(names(data())[ sapply(data(), is.factor)]) == 1){ # if only 1 factor variable present
     
     if(input$facetRow == 1 & input$facetCol == 0){
       shinyjs::enable('facetRow')
       shinyjs::disable('facetCol')
       updateSelectInput(session, inputId = "selectFacetRow", choices=facet.choices, selected = 'None')
       
     } else if(input$facetRow == 0 & input$facetCol == 1){
       shinyjs::disable('facetRow')
       shinyjs::enable('facetCol')
       updateSelectInput(session, inputId = "selectFacetCol", choices=facet.choices, selected = 'None')
       
     }
   } else { # if more than 1 factor variables present; populate both dropdowns
       shinyjs::enable('facetRow')
       shinyjs::enable('facetCol')
       updateSelectInput(session, inputId = "selectFacetRow", choices=facet.choices, selected = 'None')
       updateSelectInput(session, inputId = "selectFacetCol", choices=facet.choices, selected = 'None')
   }
    
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
  
  #_____3.7.1 SERVER : Hide Axis labels ObserveEvent (to disable axis label angle sliderInput) ---------
  
  observeEvent(input$hideAxisLabels, {
    if(input$hideAxisLabels == 1){
      shinyjs::disable('axisLabelAngle')
    } else {
      shinyjs::enable('axisLabelAngle')
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
    
    shinyjs::enable(id='update_bttn')
    
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
    shinyjs::hide("lineSize")
    shinyjs::hide("addJitter")
    #________4.0.0.2 Showing Bar specific advance options
    shinyjs::show("barplot_extra_param")
    
    
    
    #______4.0.1 GGPLOT Code--------------------
    
    # shinyjs::toggleElement('barplot_div')
    print(input$hideAxisLabels)
    list_both$plot <- bar_plot(data = data(),
                               df_name = if(input$tableName=='None'){"NULL"} else{input$tableName},
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
                               hideAxis = input$hideAxisLabels,
                               axisAngle = input$axisLabelAngle,
                               facetRow = if(input$facetRow != 1){'None'}else{input$selectFacetRow},
                               facetCol = if(input$facetCol != 1){'None'}else{input$selectFacetCol})$plot
    
    
    #______4.0.2 GGPLOT Code--------------------
    list_both$code <-
      bar_plot(data = data(),
               df_name = if(input$tableName=='None'){"NULL"} else{input$tableName},
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
               hideAxis = input$hideAxisLabels,
               axisAngle = input$axisLabelAngle,
               facetRow = if(input$facetRow != 1){'None'}else{input$selectFacetRow},
               facetCol = if(input$facetCol != 1){'None'}else{input$selectFacetCol})$code
    
    
    
    
    
  })
  
  #___4.1 PLOTS CODE: Histogram Plot Code--------------------
  
  observeEvent(input$Histogram,{
    
    shinyjs::enable(id='update_bttn')
    
    
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
    shinyjs::hide("lineSize")
    shinyjs::hide("addJitter")
    shinyjs::hide("scatter_extra_params")
    
    #______4.1.0 Plot Code--------------------
    
    list_both$plot <-
      histogram(data = data(),
                df_name = if(input$tableName=='None'){"NULL"} else{input$tableName},
                x=input$selectX,
                Theme = if(input$themeSelect=='None'){"NULL"} else{input$themeSelect},
                plotTitle = input$titleTextBox,
                fontSize = input$axisFont,
                colourfill = input$colfill,
                colorby = input$colorby,
                legendPos = input$legendPosition,
                title_x = input$titleX,
                title_y = input$titleY,
                hideAxis = input$hideAxisLabels,
                axisAngle = input$axisLabelAngle,
                bin=input$bin,
                binwidth=input$binwidth,
                facetRow = if(input$facetRow != 1){'None'}else{input$selectFacetRow},
                facetCol = if(input$facetCol != 1){'None'}else{input$selectFacetCol})$plot
    
    
    
    #______4.1.1 GGPLOT Code--------------------
    
    list_both$code <-
      histogram(data = data(),
                df_name = if(input$tableName=='None'){"NULL"} else{input$tableName},
                x=input$selectX,
                Theme = if(input$themeSelect=='None'){"NULL"} else{input$themeSelect},
                plotTitle = input$titleTextBox,
                fontSize = input$axisFont,
                colourfill = input$colfill,
                colorby = input$colorby,
                legendPos = input$legendPosition,
                title_x = input$titleX,
                title_y = input$titleY,
                hideAxis = input$hideAxisLabels,
                axisAngle = input$axisLabelAngle,
                bin=input$bin,
                binwidth=input$binwidth,
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
    shinyjs::hide("lineSize")
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
                   hideAxis = input$hideAxisLabels,
                   axisAngle = input$axisLabelAngle,
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
                   hideAxis = input$hideAxisLabels,
                   axisAngle = input$axisLabelAngle,
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
    shinyjs::show("lineSize")
    
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
                hideAxis = input$hideAxisLabels,
                axisAngle = input$axisLabelAngle,
                lineType = input$lineplot_extra_param,
                lineSize = input$lineSize,
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
                hideAxis = input$hideAxisLabels,
                axisAngle = input$axisLabelAngle,
                lineType = input$lineplot_extra_param,
                lineSize = input$lineSize,
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
    shinyjs::hide("lineSize")
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
                 hideAxis = input$hideAxisLabels,
                 axisAngle = input$axisLabelAngle,
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
                 hideAxis = input$hideAxisLabels,
                 axisAngle = input$axisLabelAngle,
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
  output$clip <- renderUI({
    rclipButton("clipbtn", "Copy", list_both$code, icon("clipboard"))
    
  })

  output$code <- renderUI({
    
    if (is.null(list_both$code)) return()
    isolate({
      
      list_both$code
    })
  })
  
  
  
} # server ends here

