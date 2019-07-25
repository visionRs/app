# UI script
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyFiles)
library(shinycssloaders)
library(htmltools)
library(ggplot2)
library(R.utils)
library(dplyr)
library(tidyverse)
library(colourpicker)

ui <- dashboardPage(
  # 1. HEADER here----
  dashboardHeader(title = "ggQuickPlot",
                  titleWidth = 300,
                  tags$li(id="refresh1",a(onclick = "window.location.href=window.location.href",
                                          href = NULL,
                                          tags$p(tags$i(class="fa fa-refresh fa-spin",style="font-size:18px"),HTML("&nbsp;")," Reload Data"),
                                          #HTML('<i class="fa fa-refresh fa-spin" style="font-size:18px"></i>'),
                                          title = "Refresh",
                                          style = "cursor: pointer; font-size: 16px;, face:bold;"),
                          class = "dropdown",
                          tags$script(HTML("
                                           var openTab = function(tabName){
                                           $('a', $('.sidebar')).each(function() {
                                           if(this.getAttribute('data-value') == tabName) {
                                           this.click()
                                           };
                                           });
                                           }")))
                  ),
  # 2. SIDEBAR here----
  dashboardSidebar(sidebarMenu(
                        # Setting id makes input$tabs give the tabName of currently-selected tab
                        id = "tabs",
                        menuItem("Home", tabName = "summary", icon = icon("dashboard"))
                        
                      ),collapsed = T
  ),
  
  # 3. BODY here----
  dashboardBody(
    includeCSS("../www/template.css"),
    shinyjs::useShinyjs(),
    
    tabItem("summary",
            
            #__3.1 Info boxes ---------------
            fluidRow(
              div(class="input_box", 
                  box(status = "primary",width = "3",
                      h4("Select input params:"),
                      
                      switchInput(inputId = "read_dt", 
                                  value = TRUE,
                                  label = "Upload File",
                                  size = "mini",
                                  width = '100px'
                                  ),
                      
                      
                      fileInput("file1", "Upload data file (csv/txt/tsv):",
                                accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")
                      ),
                      selectInput(
                        inputId = "tableName", label = "Select R Env Data ",
                        selected = "None", multiple = FALSE,
                        choices = c(temp,"None")),
                      
                      
                      #__3.2 DropDowns for X and Y axis-------------
                      selectInput(inputId = "selectX", label = "Select X-axis variable:", choices = 'None',selected = "None"),
                      selectInput(inputId = "selectY", label = "Select Y-axis variable:", choices = 'None',selected = "None"),
                      

                      
                      #__3.3 interactive or no?------------
                      materialSwitch(
                        inputId = "interact",
                        label = "Interactive plot", 
                        status = "primary",
                        right=T
                        
                      ),
                      
                      tags$h4(tags$b("Code :") ,tags$br(),tags$br(),verbatimTextOutput('return_code'),color = '#3c8dbc')
                  )
                  
              ),
              
              div(class="output_box", 
                  #tabBox(status="info","Plot Output", withSpinner(plotOutput('basic_barplot',height = '600px'),color = '#3c8dbc'),value = 'basic_barplot')
                  tabBox( height="645px",width="6",title = "",
                        
                          tabPanel(radioGroupButtons(
                            inputId = "radioPlot",
                            choices = c("Bar", "Scatter", "Line"),
                            justified = TRUE,
                            checkIcon = list(yes = icon("ok", 
                                                        lib = "glyphicon")),
                            selected = F,
                            status = "warning"
                          ),
                           withSpinner(plotOutput('basic_barplot',height = '563px'),color = '#3c8dbc'),value = 'plot6')
                          
                  ) # end of tabBox 
              ),#end of div
              div(class="input_box", 
                  box(status = "primary",width="3",
                      tags$h3("Set parameters"),
                      selectInput(inputId = 'colorby', label = 'Color by', choices = c('None')),
                      sliderInput(inputId = 'axisFont', label = 'Font Size', value = 10, min = 1, max = 50),
                      radioButtons(inputId = "rename_axes", label = "Rename axes",choices = c("yes" = 1, "no" = 0), selected = 0),
                      conditionalPanel(condition = "input.rename_axes == 1",
                        textInput(inputId = "titleX", label = 'X axis title', value = ''),
                        textInput(inputId = "titleY", label = 'Y axis title', value = '')
                      ),
                      radioGroupButtons(
                        inputId = "legendPosition",
                        label = "Legend Position",
                        choices = c("top"='top', 
                                    "bottom"='bottom', 
                                    "left"='left', 
                                    "right"='right'),
                        selected = 'right',
                        justified = TRUE,
                        checkIcon = list(
                          yes = icon("ok", 
                                     lib = "glyphicon"))
                      ),
                      conditionalPanel(condition = "input.radioPlot == 'Bar'",
                                       selectInput(inputId = 'test1', label = 'Barplot specific Input', choices = c('None'))
                      ),
                      conditionalPanel(condition = "input.radioPlot == 'Scatter'",
                                       sliderInput(inputId = 'dotSize', label = 'Dot Size', value = 2, min = 1, max = 20),
                                       sliderInput(inputId = 'dotOpa', label = 'Dot opacity', value = 0.7, min = 0, max = 1)
                      )
        
                  )
                  
              )
            ) #end of fluid row
            
    ) #end of tabItems Summary
    
  ) # dashboard Body ends here
) # UI ends here