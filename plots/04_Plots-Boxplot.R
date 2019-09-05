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
                     jitter,
                     hideAxis = 0,
                     axisAngle = 0,
                     facetRow,
                     facetCol) {
  
  
  p <- ggplot(data, aes_string(paste0("factor(",x,")"),y, fill = ifelse(colorby == 'None','NULL', colorby) )) 
    
  if(colorby == "None"){
    p <- p + geom_boxplot(fill = colourfill)
  } else{
    p <- p + geom_boxplot()
  }
  
  p <-  p +
    eval(parse(text=as.character(Theme))) +
    labs(title = plotTitle) +
    xlab(title_x) + ylab(title_y) +
    theme(axis.text = element_text(size = fontSize),
          axis.title.x = element_text(size = fontSize),
          axis.title.y = element_text(size = fontSize),
          plot.title = element_text(size = fontSize),
          legend.position = legendPos)
  
  if(jitter == TRUE){
    p <-  p + geom_jitter()
  }
  
  code <- paste0('ggplot(data,', 'aes(', x, ',', y, ifelse(colorby=="None", ')) +', paste0(',' ,'fill = ',colorby, ')) +')),
                  'geom_boxplot(', ifelse(colorby=='None', paste0('fill = ', '"' ,colourfill,'"',') +'),') +'),
                 ifelse(Theme=="NULL" | is.null(Theme),'',paste0(Theme,'+ ')),
                 ifelse(plotTitle=='' | is.null(plotTitle),'',paste0('labs(title = ','"',plotTitle,'"',') + ')),
                 ifelse(title_x=='' | is.null(title_x),'',paste0(' xlab(','"',title_x,'"',') + ')),
                 ifelse(title_y=='' | is.null(title_y),'',paste0(' ylab(','"',title_y,'"',') ')),
                 ifelse(fontSize==10 & legendPos == 'right' ,'',paste0('+ theme(axis.text = element_text(size = ', fontSize,'),
                                                                       axis.title.x = element_text(size = ', fontSize,'),
                                                                       axis.title.y = element_text(size = ', fontSize,'),
                                                                       plot.title = element_text(size = ',fontSize,'),
                                                                       legend.position = ','"',legendPos,'"',')')))
  
  # facet
  if(facetRow != 'None' & facetCol != 'None'){
    p <-  p + facet_grid(as.formula(paste0(facetRow, "~", facetCol)))
    code <- paste0(code,'+ facet_grid(',facetRow,' ~ ',facetCol,')')
  }
  if(facetRow != 'None' & facetCol == 'None'){
    p <-  p + facet_grid(as.formula(paste0(facetRow, "~ .")))
    code <- paste0(code,'+ facet_grid(',facetRow,' ~ .)')
  }
  if(facetRow == 'None' & facetCol != 'None'){
    p <-  p + facet_grid(as.formula(paste0(". ~", facetCol)))
    code <- paste0(code,'+ facet_grid(. ~ ',facetCol,')')
  }
  

  if(hideAxis == TRUE){
    p <-  p + theme(axis.text.x = element_blank())
    code <- paste0(code, '+ theme(axis.text.x = element_blank())')
  } else{
    if(axisAngle > 0){
      p <-  p + theme(axis.text.x = element_text(angle = axisAngle, hjust = 1))
      code <- paste0(code, '+ theme(axis.text.x = element_text(angle = ', axisAngle, ', hjust = 1))')
    }
  }
  
  
  ls <- list()
  ls[['plot']] <- p
  ls[['code']] <- code
  return(ls)
}