histogram <- function(data=NULL,
                      x=NULL, 
                      Theme=NULL, 
                      fontSize=10, 
                      colourfill = '#00FF0080',
                      colorby = 'None',
                      legendPos = 'right',
                      title_x='', 
                      title_y='',
                      plotTitle='') {
  
  
  p <-  ggplot(data, aes_string(x,fill =ifelse(colorby == 'None', shQuote("None"), colorby))) +
    geom_histogram() +
    eval(parse(text=as.character(Theme))) +
    labs(title = plotTitle) +
    xlab(title_x) +
    ylab(title_y) +
    theme(axis.text = element_text(size = fontSize),
          axis.title.x = element_text(size = fontSize),
          axis.title.y = element_text(size = fontSize),
          plot.title = element_text(size = fontSize),
          legend.position = legendPos)
    
  
if(colorby=='None'){
  
  p <- p + scale_fill_manual(values = colourfill) + theme(legend.position = 'none')
}
  code <- paste0('ggplot(',deparse(substitute(data)), ', aes(', x, ')) + 
                        geom_histogram() +
                        ',Theme,'() +
                        labs(title = ','"',plotTitle,'"',') +
                        xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
                        theme(axis.text = element_text(size = ', fontSize,'),
                              axis.title.x = element_text(size = ', fontSize,'),
                              axis.title.y = element_text(size = ', fontSize,'),
                              plot.title = element_text(size = ',fontSize,')')
  
  code <-paste0('ggplot(data,', 'aes(', x, ifelse(colorby=="None",paste0(', fill=', colourfill, ')) + '),paste0(',' ,'fill = ',colorby, ')) +')),' 
                geom_bar(stat="identity") +',
                ifelse(Theme=="NULL" | is.null(Theme),'',paste0(Theme,'+ ')),
                ifelse(plotTitle=='' | is.null(plotTitle),'',paste0('labs(title = ','"',plotTitle,'"',') + ')),
                ifelse(title_x=='' | is.null(title_x),'',paste0(' xlab(','"',title_x,'"',') + ')),
                ifelse(title_y=='' | is.null(title_y),'',paste0(' ylab(','"',title_y,'"',') ')),
                ifelse(fontSize==10 & legendPos == 'right' ,'',paste0('+ theme(axis.text = element_text(size = ', fontSize,'),
                                                                      axis.title.x = element_text(size = ', fontSize,'),
                                                                      axis.title.y = element_text(size = ', fontSize,'),
                                                                      plot.title = element_text(size = ',fontSize,'),
                                                                      legend.position = ','"',legendPos,'"',')'))
                )
  
  ls <- list()
  ls[['plot']] <- p
  ls[['code']] <- code
  return(ls)
  
}