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
      
      # update textInputs for renaming axes
      updateTextInput(session = session,inputId = "titleX", value = input$selectX)
      updateTextInput(session = session,inputId = "titleY", value = input$selectY)
    })
    
    #4 PLOTS CODE: -------------
    output$basic_barplot <- renderPlot({
      dt <- data()
      if(is.null(dt)){return()}
      if(is.null(input$radioPlot)){return()}
      
      switch(input$radioPlot,
             "Bar" =    if(!(is.null(dt[[input$selectX]]) | is.null(dt[[input$selectY]]))){
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
                        } else if(is.null(dt[[input$selectX]]) & is.null(dt[[input$selectY]]))
                        {
                          sendSweetAlert(
                            session = session,
                            title = "Error !!",
                            text = "It's broken...",
                            type = "error"
                          )
                          
                        } else if(!is.null(dt[[input$selectX]])){  
                               histogram(data = dt,
                                   x=input$selectX,
                                   y=input$selectY,
                                   Theme = input$themeSelect,
                                   plotTitle = input$titleTextBox,
                                   fontSize = input$axisFont, 
                                   title_x = input$titleX,
                                   title_y = input$titleY)$plot
                 
                        } else {
                          sendSweetAlert(
                            session = session,
                            title = "Error !!",
                            text = "It's broken...",
                            type = "error"
                          )
                          
                        },
             
             "Scatter" = scatter_plot(data = dt,
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
                                      correlation = input$corr)$plot,
             
             "Line" =    line_plot(data = dt,
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
             
      )
      
    })
    
    
    #5 RETURN CODE BLOCK: -------------
    output$return_code <- renderText({
      dt <- data()
      if(is.null(dt)){return()}
      if(is.null(input$radioPlot)){return()}
      
      switch(input$radioPlot,
             
             "Bar" =    if(!(is.null(dt[[input$selectX]]) | is.null(dt[[input$selectY]]))){
               bar_plot(data = dt,
                        x=input$selectX,
                        y=input$selectY,
                        Theme = input$themeSelect,
                        plotTitle = input$titleTextBox,
                        #colourfill = if(input$colorby=="None"){input$colfill} else {"NULL"}, 
                        colorby = if(input$colorby=="None"){"NULL"} else {input$colorby}, 
                        fontSize = input$axisFont, 
                        legendPos = input$legendPosition,
                        title_x = input$titleX,
                        title_y = input$titleY)$code
             } else if(is.null(dt[[input$selectX]]) & is.null(dt[[input$selectY]]))
             {
               print("Both can't be None")
               
             } else if(!is.null(dt[[input$selectX]])){  
               histogram(data = dt,
                         x=input$selectX,
                         y=input$selectY, 
                         Theme = input$themeSelect,
                         plotTitle = input$titleTextBox,
                         fontSize = input$axisFont, 
                         title_x = input$titleX,
                         title_y = input$titleY)$code
               
             } else {
               print("X can't be None.")
               
             },

             "Scatter" = scatter_plot(data = dt,
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
                                      correlation = input$corr)$code,
             
             "Line" =    line_plot(data = dt,
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
      )
      
    })
    
} # server ends here
  
