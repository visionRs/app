#1. Sidebar Code------------
library(shinydashboard)
library(htmltools)
library(shiny)
sidebar <-   dashboardSidebar(sidebarMenu(
                                # Setting id makes input$tabs give the tabName of currently-selected tab
                                id = "tabs",
                                menuItem("Home", tabName = "summary", icon = icon("dashboard"))

                              ),collapsed = T

)



