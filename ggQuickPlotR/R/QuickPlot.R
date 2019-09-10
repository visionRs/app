QuickPlot <- function(data = NULL, 
                      viewer = getOption(x = "esquisse.viewer", default = "dialog")) {
  
  if (viewer == "browser") {
    inviewer <- browserViewer(browser = getOption("browser"))
  } else if (viewer == "pane") {
    inviewer <- paneViewer(minHeight = "maximize")
  } else {
    inviewer <- dialogViewer(
      "ggQuickPlot",
      width = 1500, height = 1000
    )
  }
  
  runGadget(
    app = ui, 
    server = server, 
    viewer = inviewer
  )
}