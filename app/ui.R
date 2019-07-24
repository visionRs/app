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


ui <- dashboardPage(
  # 1. HEADER here----
  dashboardHeader(title = "ggEasyPlot",
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
              div(class="input_box", style="width:50%",
                  box(status = "primary",
                      h4("Select input params:"),
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
                      selectInput(inputId = "selectX", label = "Select X-axis variable:", choices = ''),
                      selectInput(inputId = "selectY", label = "Select Y-axis variable:", choices = ''),
                      
                      #__3.3 Add radios to choose type of plot------------
                      radioGroupButtons(
                        inputId = "radioPlot",
                        label = "Select Plot Type",
                        choices = c("Bar", "Scatter", "Line"),
                        justified = TRUE,
                        checkIcon = list(yes = icon("ok", 
                                                    lib = "glyphicon")),
                        selected = F,
                        status = "warning"
                      ),
                      
                      #__3.4 interactive or no?------------
                      prettySwitch(
                        inputId = "interact",
                        label = "Interactive plot", 
                        status = "primary",
                        slim = TRUE
                      ),
                      
                      tags$h4(tags$b("Code :") ,tags$br(),tags$br(),verbatimTextOutput('return_code'),color = '#3c8dbc')
                  )
                  
              ),
              
              div(class="output_box", style="width:150%;",
                  #tabBox(status="info","Plot Output", withSpinner(plotOutput('basic_barplot',height = '600px'),color = '#3c8dbc'),value = 'basic_barplot')
                  tabBox( height = '663px',title = 
                            dropdownButton(
                              tags$h3("Set parameters"),
                              #selectInput(inputId = 'xcol', label = 'X Variable', choices = names(iris)),
                              selectInput(inputId = 'colorby', label = 'Color by', choices = c('None')),
                              #selectInput(inputId = 'selectTheme', label = 'Choose Theme', choices = c(theme_bw(), theme_classic()), selected = theme_bw()),
                              sliderInput(inputId = 'axisFont', label = 'Font Size', value = 10, min = 1, max = 50),
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
                              ),
                              #selectInput(inputId = 'ycol', label = 'Y Variable', choices = names(iris), selected = names(iris)[[2]]),
                              #sliderInput(inputId = 'clusters', label = 'Cluster count', value = 3, min = 1, max = 9),
                              circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
                              tooltip = tooltipOptions(title = "Click to see inputs !")
                            ),
                          tabPanel(tagList(shiny::icon("table"),""), withSpinner(plotOutput('basic_barplot',height = '563px'),color = '#3c8dbc'),value = 'plot6')
                          
                  ) # end of tabBox 
              )#end of div
              
            ) #end of fluid row
            
    ) #end of tabItems Summary
    
  ) # dashboard Body ends here
) # UI ends here