# SERVER SCRIPT

#1. FETCH DATA from Envir------------------------

# 2. SOURCE all plot scripts-----------------------

# 3. SERVER Starts Here------------------------
  
  server = function(input, output, session) {
    
    #___3.1 SERVER: Refresh Function--------------
    observeEvent(input$refresh1, {
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
          get(input$tableName, envir = .GlobalEnv)
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
    
    observeEvent(c(input$selectX,input$selectY), {
      dt <- data()
      if((!is.numeric(dt[[input$selectX]]) & !is.numeric(dt[[input$selectY]])) | (is.null(dt[[input$selectX]]) & is.null(dt[[input$selectY]])) | is.null(dt[[input$selectX]])){
        shinyjs::disable("Bar")
        shinyjs::disable("Histogram")
        shinyjs::disable("Scatter")
        shinyjs::disable("Line")
        
        
      } else if((is.null(dt[[input$selectY]]) & is.numeric(dt[[input$selectX]]))) {
        
        shinyjs::disable("Bar")
        shinyjs::disable("Scatter")
        shinyjs::disable("Line")
        shinyjs::enable("Histogram")
        
       
      } else if(!is.numeric(dt[[input$selectX]]) | !is.numeric(dt[[input$selectY]])) {
        
        shinyjs::enable("Bar")
        shinyjs::disable("Scatter")
        shinyjs::enable("Line")
        shinyjs::disable("Histogram")
        
                  
      } else {
        
        shinyjs::enable("Bar")
        shinyjs::enable("Scatter")
        shinyjs::enable("Line")
        shinyjs::disable("Histogram")
      
      }
      
      # update textInputs for renaming axes
      updateTextInput(session = session,inputId = "titleX", value = input$selectX)
      updateTextInput(session = session,inputId = "titleY", value = input$selectY)
    })
    
    #4 PLOTS CODE: -------------
    
 
    #___4.0 PLOTS CODE: Bar Plot Code-----------------
    observeEvent(input$Bar,ignoreInit  =T ,{
    if(is.null(input$Bar)) return()
      isolate({
        #______4.0.0 GGPLOT Code--------------------
        
      shinyjs::toggleElement('barplot_div')
        
       output$plot <-
         renderPlot({
           dt <- data()
           bar_plot(data = dt,
                 x=input$selectX,
                 y=input$selectY,
                 plotTitle = input$titleTextBox,
                 Theme = input$themeSelect,
                 #colorfill = if(input$colorby=="None"){input$colfill} else {"NULL"},
                 colorby = if(input$colorby=="None"){"NULL"}else{input$colorby} ,
                 fontSize = input$axisFont,
                 legendPos = input$legendPosition,
                 title_x = input$titleX,
                 title_y = input$titleY)$plot

         })
       
       #______4.0.1 GGPLOT Code--------------------
       output$code <-
         renderText({
           dt <- data()
           bar_plot(data = dt,
                    x=input$selectX,
                    y=input$selectY,
                    plotTitle = input$titleTextBox,
                    Theme = input$themeSelect,
                    #colorfill = if(input$colorby=="None"){input$colfill} else {"NULL"},
                    colorby = if(input$colorby=="None"){"NULL"}else{input$colorby} ,
                    fontSize = input$axisFont,
                    legendPos = input$legendPosition,
                    title_x = input$titleX,
                    title_y = input$titleY)$code
           
         })
       
       
        })
    })
    
    #___4.1 PLOTS CODE: Histogram Plot Code--------------------
    
    observeEvent(input$Histogram,ignoreInit  =T,{
      if(is.null(input$Histogram)) return()
      isolate({
        #______4.1.0 Plot Code--------------------
        
        output$plot <-
          renderPlot({
            dt <- data()
            histogram(data = dt,
                       x=input$selectX,
                       Theme = input$themeSelect,
                       plotTitle = input$titleTextBox,
                       fontSize = input$axisFont,
                       title_x = input$titleX,
                       title_y = input$titleY)$plot
            
          })
        
        #______4.1.1 GGPLOT Code--------------------
        
        output$code <-
          renderText({
            dt <- data()
            histogram(data = dt,
                      x=input$selectX,
                      Theme = input$themeSelect,
                      plotTitle = input$titleTextBox,
                      fontSize = input$axisFont,
                      title_x = input$titleX,
                      title_y = input$titleY)$code
            
          })
      })
        
      
    })
       

    #___4.2 PLOTS CODE: Scatter Plot Code------------------
    
    observeEvent(input$Scatter,ignoreNULL = T,{
      if(is.null(input$Scatter)) return()
      isolate({
        shinyjs::toggleElement('regression_div')
        
        #______4.2.0 Plot Code--------------------
        output$plot <-
          renderPlot({
            dt <- data()
            scatter_plot(data = dt,
                         x=input$selectX,
                         y=input$selectY,
                         Theme = input$themeSelect,
                         plotTitle = input$titleTextBox,
                         colourfill = input$colfill,
                         colorby = input$colorby,
                         fontSize = input$axisFont,
                         legendPos = input$legendPosition,
                         dotSize = input$dotSize,
                         dotOpa = input$dotOpa,
                         title_x = input$titleX,
                         title_y = input$titleY,
                         regressionLine = input$regLine,
                         correlation = input$corr)$plot
            
          })
        #______4.2.1 GGPLOT Code--------------------
        output$code <- renderText({
          dt <- data()
          scatter_plot(data = dt,
                       x=input$selectX,
                       y=input$selectY,
                       Theme = input$themeSelect,
                       plotTitle = input$titleTextBox,
                       colourfill = input$colfill,
                       colorby = input$colorby,
                       fontSize = input$axisFont,
                       legendPos = input$legendPosition,
                       dotSize = input$dotSize,
                       dotOpa = input$dotOpa,
                       title_x = input$titleX,
                       title_y = input$titleY,
                       regressionLine = input$regLine,
                       correlation = input$corr)$code
          
          
        })
      })
    })
    
    #___4.3 PLOTS CODE: Line Plot Code------------------
    
    observeEvent(input$Line,ignoreInit  =T,{
      if(is.null(input$Line)) return()
      isolate({
        
        #______4.3.0 Plot Code--------------------
        output$plot <-
          renderPlot({
            dt <- data()
            line_plot(data = dt,
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
            
          })
        #______4.3.1 GGPLOT Code--------------------
        output$code <- renderText({
          dt <- data()
          line_plot(data = dt,
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
      })
    })
    
} # server ends here
  
