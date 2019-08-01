#1 PLOTS CODE: --------------------------------

#1.1 PLOTS CODE: Bar Plot---------------------

box_plot <- function(data=NULL,
                     x=NULL,
                     y=NULL,
                     Theme=NULL, 
                     colourfill, 
                     colorby, 
                     fontSize, 
                     legendPos,
                     title_x, 
                     title_y, 
                     plotTitle,
                     jitter) {
  
  p <- ggplot(data, aes_string(paste0("factor(",x,")"),y, fill = ifelse(colorby == 'None','NULL',colorby) ))
    
  
  if(colorby == "None"){
    
    p <- p + geom_boxplot(fill = colourfill) +
      eval(parse(text=as.character(Theme))) +
      labs(title = plotTitle) +
      xlab(title_x) + ylab(title_y) +
      theme(axis.text = element_text(size = fontSize),
            axis.title.x = element_text(size = fontSize),
            axis.title.y = element_text(size = fontSize),
            plot.title = element_text(size = fontSize),
            legend.position = legendPos)
  } else{
    p <- p + geom_boxplot() +
      eval(parse(text=as.character(Theme))) +
      labs(title = plotTitle) +
      xlab(title_x) + ylab(title_y) +
      theme(axis.text = element_text(size = fontSize),
            axis.title.x = element_text(size = fontSize),
            axis.title.y = element_text(size = fontSize),
            plot.title = element_text(size = fontSize),
            legend.position = legendPos)
  }
  
  if(jitter == TRUE){
    p <-  p + geom_jitter()
  }
  
  code <- paste0('ggplot(data,', 'aes(', x, ',', y, ifelse(colorby=='None',')) +', paste0(',' ,'fill = ',colorby, ')) +')),' 
                  geom_boxplot(', ifelse(colorby=='None', paste0('fill = ', '"' ,colourfill,'"',') +'),') +'),
                 ifelse(Theme=="NULL" | is.null(Theme),'',paste0(Theme,'+ ')),
                 ifelse(plotTitle=='' | is.null(plotTitle),'',paste0('labs(title = ','"',plotTitle,'"',') + ')),
                 ifelse(title_x=='' | is.null(title_x),'',paste0(' xlab(','"',title_x,'"',') + ')),
                 ifelse(title_y=='' | is.null(title_y),'',paste0(' ylab(','"',title_y,'"',') ')),
                 ifelse(fontSize==10 & legendPos == 'right' ,'',paste0('+ theme(axis.text = element_text(size = ', fontSize,'),
                                                                       axis.title.x = element_text(size = ', fontSize,'),
                                                                       axis.title.y = element_text(size = ', fontSize,'),
                                                                       plot.title = element_text(size = ',fontSize,'),
                                                                       legend.position = ','"',legendPos,'"',')')))
  
  ls <- list()
  ls[['plot']] <- p
  ls[['code']] <- code
  return(ls)
}