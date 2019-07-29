#1 PLOTS CODE: --------------------------------

#1.1 PLOTS CODE: Bar Plot---------------------

bar_plot <- function(data=NULL,x=NULL,y=NULL, colorby=NULL, Theme, fontSize, legendPos, title_x, title_y, plotTitle) {
  if(Theme == 'None'){
    p <- ggplot(data, aes_string(x, y, fill = colorby)) +
      geom_bar(stat="identity") +
      labs(title = plotTitle) +
      xlab(title_x) + ylab(title_y) +
      theme(axis.text = element_text(size = fontSize),
            axis.title.x = element_text(size = fontSize),
            axis.title.y = element_text(size = fontSize),
            plot.title = element_text(size = fontSize),
            legend.position = legendPos)
    
    code <- paste0('ggplot(data, aes(', x, ',', y, ',' ,'fill = ', colorby, ')) + 
                   geom_bar(stat="identity") +
                   labs(title = ','"',plotTitle,'"',') +
                   xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
                   theme(axis.text = element_text(size = ', fontSize,'),
                   axis.title.x = element_text(size = ', fontSize,'),
                   axis.title.y = element_text(size = ', fontSize,'),
                   plot.title = element_text(size = ',fontSize,'),
                   legend.position = ','"',legendPos,'"',')')
    ls <- list()
    ls[['plot']] <- p
    ls[['code']] <- code
    return(ls)
    
  } else{
    p <- ggplot(data, aes_string(x, y, fill = colorby)) +
      geom_bar(stat="identity") +
      get(Theme)() +
      labs(title = plotTitle) +
      xlab(title_x) + ylab(title_y) +
      theme(axis.text = element_text(size = fontSize),
            axis.title.x = element_text(size = fontSize),
            axis.title.y = element_text(size = fontSize),
            plot.title = element_text(size = fontSize),
            legend.position = legendPos)
    
    code <- paste0('ggplot(data, aes(', x, ',', y, ',' ,'fill = ', colorby, ')) + 
                   geom_bar(stat="identity") +
                    ',Theme,'() +
                    labs(title = ','"',plotTitle,'"',') +
                    xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
                   theme(axis.text = element_text(size = ', fontSize,'),
                   axis.title.x = element_text(size = ', fontSize,'),
                   axis.title.y = element_text(size = ', fontSize,'),
                    plot.title = element_text(size = ',fontSize,'),
                   legend.position = ','"',legendPos,'"',')')
    ls <- list()
    ls[['plot']] <- p
    ls[['code']] <- code
    return(ls)
  }# else ends for Theme
  }


