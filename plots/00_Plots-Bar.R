#1 PLOTS CODE: --------------------------------

#1.1 PLOTS CODE: Bar Plot---------------------

bar_plot <- function(data=NULL,x=NULL,y=NULL, colorby, fontSize, legendPos, title_x, title_y) {
  if(colorby == 'None'){
    p <- ggplot(data, aes_string(x, y)) +
      geom_bar(stat="identity") +
      xlab(title_x) + ylab(title_y) +
      theme(axis.text = element_text(size = fontSize),
            axis.title.x = element_text(size = fontSize),
            axis.title.y = element_text(size = fontSize))
    
    code <- paste0('ggplot(data, aes(',x,',',y,')) +
              geom_bar(stat="identity") +
              xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
            theme(axis.text = element_text(size = ', fontSize,'),
            axis.title.x = element_text(size = ', fontSize,'),
            axis.title.y = element_text(size = ', fontSize,'))')
    
    ls <- list()
    ls[['plot']] <- p
    ls[['code']] <- code
    return(ls)
  } else{
    p <- ggplot(data, aes_string(x, y, fill = colorby)) +
      geom_bar(stat="identity") +
      xlab(title_x) + ylab(title_y) +
      theme(axis.text = element_text(size = fontSize),
            axis.title.x = element_text(size = fontSize),
            axis.title.y = element_text(size = fontSize),
            legend.position = legendPos)
    
    code <- paste0('ggplot(data, aes(', x, ',', y, ',' ,'fill = ', colorby, ')) + 
                   geom_bar(stat="identity") +
                    xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
                   theme(axis.text = element_text(size = ', fontSize,'),
                   axis.title.x = element_text(size = ', fontSize,'),
                   axis.title.y = element_text(size = ', fontSize,'),
                   legend.position = ','"',legendPos,'"',')')
    ls <- list()
    ls[['plot']] <- p
    ls[['code']] <- code
    return(ls)
  }
}
