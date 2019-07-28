histogram <- function(data=NULL,x=NULL, Theme, fontSize, title_x, title_y,plotTitle) {
  
  
  p <-  ggplot(data, aes_string(x)) +
    geom_histogram() +
    get(Theme)() +
    labs(title = plotTitle) +
    xlab(title_x) +
    ylab(title_y) +
    theme(axis.text = element_text(size = fontSize),
          axis.title.x = element_text(size = fontSize),
          axis.title.y = element_text(size = fontSize),
          plot.title = element_text(size = fontSize)
    )
  
  code <- paste0('ggplot(',deparse(substitute(data)), ', aes(', x, ')) + 
                        geom_histogram() +
                        ',Theme,'() +
                        labs(title = ','"',plotTitle,'"',') +
                        xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
                        theme(axis.text = element_text(size = ', fontSize,'),
                              axis.title.x = element_text(size = ', fontSize,'),
                              axis.title.y = element_text(size = ', fontSize,'),
                              plot.title = element_text(size = ',fontSize,')')
  ls <- list()
  ls[['plot']] <- p
  ls[['code']] <- code
  return(ls)
  
}