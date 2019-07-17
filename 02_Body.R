#1. UI Body Code------------
library(shiny)
library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(shinyFiles)
library(ggplot2)
library(shinycssloaders)
library(shinyWidgets)

body <- dashboardBody(
  includeCSS("template.css"),
  
  tabItem("summary",
   
   #____4.2.0 Info boxes ---------------
   fluidRow(
    div(class="input_box", style="width:70%;",
     box(
        h4("Select input params:"),
        fileInput("file1", "Upload data file (csv/txt/tsv):",
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                  ),
       #__1.1 DropDowns for X and Y axis-------------
       selectInput(inputId = "selectX", label = "Select X-axis variable:", choices = ''),
       selectInput(inputId = "selectY", label = "Select Y-axis variable:", choices = ''),
       
       #__1.2 Add radios to choose type of plot------------
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
       
       #__1.3 interactive or no?------------
       prettySwitch(
         inputId = "interact",
         label = "Interactive plot", 
         status = "primary",
         slim = TRUE
       ),
       box("Plot Code", withSpinner(verbatimTextOutput('return_code'),color = '#3c8dbc'),value = 'return_code', width = 150)
     ) # end of Box
    ),
    
      div(class="output_plot", style="width=60%;",
       box("Plot Output", withSpinner(plotOutput('basic_barplot',height = '600px'),color = '#3c8dbc'),value = 'basic_barplot')
      )

   ) # end of fluidRow
 ) # end of tabITem 
) # end of body
  
