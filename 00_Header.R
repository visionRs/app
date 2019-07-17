# 1. Header Code:------------------
library(shinydashboard)
library(htmltools)
library(shiny)
header <- dashboardHeader(title = tags$div(id="preloader4",tags$span(),tags$span(),tags$span(),tags$span(),"GGEasyPlot"),
                          # tags$div(class="logocss",tags$a(href='http://www.vnsny.org',
                          #                tags$img(id="logocss",src='vnsnylogo.png')),"Model Evaluation Tool"),
                          # 
                         
                          tags$li(a(onclick = "openTab('Home')",
                                    href = NULL,
                                    #icon("info-circle"),"Home",
                                    title = "Home",
                                    style = "cursor: pointer;font-size: 16px;, face:bold;"),
                                  class = "dropdown",
                                  tags$script(HTML("
                                       var openTab = function(tabName){
                                       $('a', $('.sidebar')).each(function() {
                                       if(this.getAttribute('data-value') == tabName) {
                                       this.click()
                                       };
                                       });
                                       }"))),
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
                          
)