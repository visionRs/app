library(shiny)
library(shinyWidgets)

source('plots/00_Plots-R-Code.R', echo=F)
library(ggplot2)
ui <- basicPage(
  titlePanel("testApp mainpage"),
  
  sidebarLayout(
    # 1. INPUT UI CODE: ------------------
    #__1.0 Upload data file
    sidebarPanel(h4("Select input params:"),
                 fileInput("file1", "Upload data file (csv/txt/tsv):",
                           accept = c(
                             "text/csv",
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
                   choices = c("Bar", "Scatter", "Line", "Pie"),
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
                 )
    ),
    mainPanel("Resulting Data with Plot",
              plotOutput("basic_barplot"),
              uiOutput("displayCode"))
    
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
  
  
  
  # PLOTS CODE: 
  output$basic_barplot <- renderPlot({
    dt <- data()
    if(is.null(dt)){return()}
    if(is.null(input$radioPlot)){return()}
    
    switch(input$radioPlot,
           "Bar" =  bar_plot(data = dt,x=input$selectX,y=input$selectY),
           "Scatter" = scatter_plot(data = dt,x=input$selectX,y=input$selectY),
           "Line" =    line_plot(data = dt,x=input$selectX,y=input$selectY)
    )
  })
  
  
  # to display code underneath the plot
  observeEvent(input$radioPlot, {
    output$displayCode <-  renderUI({
    
    switch(input$radioPlot,
             "Bar" =  includeMarkdown("plots/barplotCode.rmd"),
             "Scatter" = includeMarkdown("plots/scatterplotCode.rmd"),
             "Line" =    includeMarkdown("plots/lineplotCode.rmd")
      )
    })
  
  }) # end of observeEvent
  
  

}# end of server

runApp(list(ui = ui, server = server))
