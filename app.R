library(shiny)
library(shinyWidgets)

ui <- basicPage(
  titlePanel("testApp mainpage"),
  
  sidebarLayout(
    sidebarPanel(h4("Select input params:"),
                 fileInput("file1", "Upload data file (csv/txt/tsv):",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                 ),
                 # DropDowns for X and Y axis
                 selectInput(inputId = "selectX", label = "Select X-axis variable:", choices = ''),
                 selectInput(inputId = "selectY", label = "Select Y-axis variable:", choices = ''),
                 
                 # Add radios to choose type of plot
                 radioGroupButtons(
                   inputId = "radioPlot",
                   label = "Select Plot Type",
                   choices = c("Bar", "Dot", "Line", "Pie"),
                   justified = TRUE,
                   checkIcon = list(yes = icon("ok", 
                                               lib = "glyphicon")),
                   selected = F,
                   status = "warning"
                 ),
                 
                 # interactive or no?
                 prettySwitch(
                   inputId = "interact",
                   label = "Interactive plot", 
                   status = "primary",
                   slim = TRUE
                 )
    ),
    mainPanel("Resulting Data with Plot",
              tableOutput("tabout"))
    
  ) # end of sidebarLayout
  
) # end of ui


server <- function(input, output,session) {
  
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
  
  
}# end of server

runApp(list(ui = ui, server = server))