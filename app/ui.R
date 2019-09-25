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
library(ggpubr)
library(rclipboard)
library(highcharter)

ui <- dashboardPage(
  # 1. HEADER here----
  dashboardHeader(title = "ggQuickPlot",
                  titleWidth = 300,
                  dropdownActionMenu(id="menu-drpdwn",title= "",icon = icon("chevron-circle-down"),
                                     actionItem("refresh1",tags$p(tags$i(class="fa fa-refresh fa-spin",style="font-size:12px"),HTML("&nbsp;")," Reload Data")),
                                     actionItem("github","Report Issue" ,icon = icon("exclamation-triangle"),onclick_event = "window.open('https://github.com/easy-plot/app/issues', '_blank')")
                  )
                  # tags$li(id="refresh1",a(onclick = "window.location.href=window.location.href",
                  #                         href = NULL,
                  #                         tags$p(tags$i(class="fa fa-refresh fa-spin",style="font-size:18px"),HTML("&nbsp;")," Reload Data"),
                  #                         #HTML('<i class="fa fa-refresh fa-spin" style="font-size:18px"></i>'),
                  #                         title = "Refresh",
                  #                         style = "cursor: pointer; font-size: 16px;, face:bold;"),
                  #         class = "dropdown",
                  #         tags$script(HTML("
                  #                          var openTab = function(tabName){
                  #                          $('a', $('.sidebar')).each(function() {
                  #                          if(this.getAttribute('data-value') == tabName) {
                  #                          this.click()
                  #                          };
                  #                          });
                  #                          }")))
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
    rclipboardSetup(),
    
    tags$head(tags$script(HTML("$(document).on('click', '.needed', function () {
                                Shiny.onInputChange('last_btn',this.id);
                               });"))),
    shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
    
    
    tabItem("summary",
            fluidRow(
              # div(style="display:inline-block;width:20%;",actionButton("Bar", "Bar",class="needed")),
              # div(style="display:inline-block;width:20%;",actionButton("Histogram", "Histogram",class="needed")),
              # div(style="display:inline-block;width:20%;",actionButton("Scatter", "Scatter",class="needed")),
              # div(style="display:inline-block;width:20%;",actionButton("Line", "Line",class="needed")),
              # div(style="display:inline-block;width:19%;",actionButton("Box", "Box",class="needed"))
              actionGroupButtons(
                inputIds = c("Bar", "Histogram", "Scatter", "Line","Box"),
                labels = list("Bar", "Histogram", "Scatter","Line","Box"),
                status = "danger",
                fullwidth = T
              )
              
            ),
            br(),
            fluidRow(
              column(
                width = 3,
                dropdownButton(
                  inputId = "color_drp",
                  label = "Color/Themes",
                  icon = icon("sliders"),
                  status = "primary",
                  circle = FALSE,
                  margin = '1px',
                  colourInput("colfill", "Color fill", "#2219CCCC", allowTransparent = TRUE),
                  selectInput(inputId = 'colorby', label = 'Color by', choices = c('None'),selected = 'None'),
                  selectInput(inputId = 'themeSelect', label = 'Choose Theme', choices = c('None'="NULL",
                                                                                           'theme_bw()',
                                                                                           'theme_gray()',
                                                                                           'theme_dark()',
                                                                                           'theme_minimal()',
                                                                                           'theme_linedraw()',
                                                                                           'theme_light()',
                                                                                           'theme_classic()'))
                  
                )
                
              ),
              
              column(
                width = 3,
                dropdownButton(
                  inputId = "axes_drp",
                  label = "Axes/Title",
                  icon = icon("sliders"),
                  status = "primary",
                  circle = FALSE, margin = "1px",
                  sliderInput(inputId = 'axisFont', label = 'Font Size', value = 10, min = 1, max = 50),
                  radioButtons(inputId = "rename_axes", label = "Rename axes",choices = c("yes" = 1, "no" = 0), selected = 0),
                  textInput(inputId = "titleTextBox", label = "Set Plot title", value = ""),
                  conditionalPanel(condition = "input.rename_axes == 1",
                                   textInput(inputId = "titleX", label = 'X axis title', value = ''),
                                   textInput(inputId = "titleY", label = 'Y axis title', value = '')
                                   
                  ),
                  checkboxInput(inputId ="hideAxisLabels", "Hide Axis Labels", value = FALSE),
                  sliderInput(inputId = 'axisLabelAngle', label = 'Axis Label Angle', value = 0, min = 0, max = 90, step = 5),
                  checkboxInput(inputId ="showFacet", "Show facets", value = FALSE),
                  conditionalPanel(condition = "input.showFacet == 1",
                                   checkboxInput(inputId ="facetRow", "Facet by row", value = FALSE),
                                   conditionalPanel(condition = "input.facetRow ==1",
                                                    selectInput(inputId = "selectFacetRow", label = "Select variable", choices = 'None')
                                   ),
                                   checkboxInput(inputId ="facetCol", "Facet by column", value = FALSE),
                                   conditionalPanel(condition = "input.facetCol ==1",
                                                    selectInput(inputId = "selectFacetCol", label = "Select variable", choices = 'None')
                                   )
                                   
                  )
                  
                )),
              column(
                width = 3,
                dropdownButton(inputId = "legend_drp",
                               label = "Legend",
                               icon = icon("sliders"),
                               status = "primary",
                               circle = FALSE,
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
                               )
                )),
              column(
                width = 3,
                dropdownButton(inputId = "extra_drp",
                               label = "Plot Specific Paramters",
                               icon = icon("sliders"),
                               status = "primary",
                               circle = FALSE,
                               conditionalPanel(condition = "input.Bar",
                                                box(id = 'bar_extra_params', width =12,
                                                selectInput(inputId = "position_input",
                                                            label = "Position",
                                                            choices = c('fill',"dodge","stack"),selected = 'stack'),
                                                checkboxInput(inputId='coorflip_input','CoordFlip',value = FALSE)
                                      
                               )),
                               conditionalPanel(condition = "input.Histogram",
                                                box(id = 'hist_extra_params', width =12,
                                                    selectInput(inputId = "position_input",
                                                                label = "Position",
                                                                choices = c('fill',"dodge","stack"),selected = 'stack'),
                                                    checkboxInput(inputId='coorflip_input','CoordFlip',value = FALSE),
                                                    checkboxInput(inputId='density_checkbox','Density Plot',value = FALSE),
                                                    conditionalPanel(condition = "input.density_checkbox",
                                                                     selectInput(inputId = "density_fill",
                                                                                 label = "Density Fill Column",
                                                                                 choices = 'None'
                                                                     ),
                                                                     numericInput(inputId="alpha_input",label = 'Alpha',value = 0.5,min=0,max=1)
                                                    )
                                                    
                                                )
                                                
                                                
                               ),
                               
                               conditionalPanel(condition = "input.Scatter",
                                                box(id = 'scatter_extra_params', width =12,
                                                    selectInput(inputId = 'shapeBy', label = 'Shape by', choices = c('None')),
                                                    sliderInput(inputId = 'dotSize', label = 'Dot Size', value = 2, min = 1, max = 20),
                                                    sliderInput(inputId = 'dotOpa', label = 'Dot opacity', value = 0.7, min = 0, max = 1),
                                                    box(title = "Regression", width = 12,
                                                        checkboxInput(inputId ="regLine", "Add regression line?", value = FALSE),
                                                        conditionalPanel(condition = "input.regLine == 1",
                                                                         #checkboxInput(inputId ="se", "Add confidence interval?", value = FALSE),
                                                                         checkboxInput(inputId ="corr", "Show correlation?", value = FALSE)
                                                        ) # end of input.regline conditional panel
                                                    )
                                                )
                               ),# end of input.scatter conditional panel  
                               conditionalPanel(condition = "input.Line",
                                                selectInput(inputId = 'lineplot_extra_param', label = 'Line type', choices = c('solid',
                                                                                                                              'twodash',
                                                                                                                              'longdash',
                                                                                                                              'dotted',
                                                                                                                              'dotdash',
                                                                                                                              'dashed'), selected = 'solid'),
                                                checkboxInput(inputId ="dotLine", "Add Points to plot", value = FALSE),
                                                sliderInput(inputId = 'lineSize', label = 'Line Size', value = 1, min = 0, max = 5, step = 0.5)
                               ),
                               conditionalPanel(condition = "input.Box",
                                                checkboxInput(inputId ="addJitter", "Add Jitter", value = FALSE)
                               )
                               
                )
                
                
              )# column ends here
              
              
              
            ), # end of fluidRow
            br(),
            
            
            #__3.1 Info boxes ---------------
            fluidRow(
              div(class="input_box", 
                  box(status = "primary",width = "3",height="750px",
                      h4("Select input params:"),
                      
                      switchInput(inputId = "read_dt", 
                                  value = FALSE,
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
                        selected = "None", multiple = F,
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
                      
                      htmlOutput('code'),
                      uiOutput("clip")
                      
                      )
                  
              ),
              
              div(class="output_box", 
                  #tabBox(status="info","Plot Output", withSpinner(plotOutput('basic_barplot',height = '600px'),color = '#3c8dbc'),value = 'basic_barplot')
                  box(width="9",height = '750px',
                      title = "",
                      shinyjs::disabled(actionBttn(
                        inputId = "update_bttn",
                        label = "Update",
                        color = "success",
                        style = "material-flat",
                        icon = icon("sliders"),
                        block = TRUE
                      )),
                      #withSpinner(plotOutput('plot',height = '650px'),color = '#3c8dbc'),
                      
                      conditionalPanel(
                        condition = 'input.interact == true',
                        withSpinner(plotlyOutput('plotly',height = '650px'),color = '#3c8dbc')
                      ),
                      
                      conditionalPanel(
                        condition = 'input.interact == false',
                        withSpinner(plotOutput('plot',height = '650px'),color = '#3c8dbc')
                      ),
                      
                      value = 'plot6',status = "primary"
                      
                  ) # end of tabBox 
              )#end of div
              
            ) #end of fluid row
            
    ) #end of tabItems Summary
    
  ) # dashboard Body ends here
) # UI ends here
