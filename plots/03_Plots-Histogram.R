histogram <- function(data=NULL,
                      df_name=NULL,
                      x=NULL, 
                      Theme=NULL, 
                      fontSize=10, 
                      colourfill = '#00FF0080',
                      colorby = 'None',
                      legendPos = 'right',
                      title_x='', 
                      title_y='',
                      plotTitle='',
                      hideAxis,
                      axisAngle,
                      facetRow,
                      facetCol) {
  
  
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
  # code <- paste0('ggplot(',deparse(substitute(data)), ', aes(', x, ')) + 
  #                       geom_histogram() +
  #                       ',Theme,'() +
  #                       labs(title = ','"',plotTitle,'"',') +
  #                       xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
  #                       theme(axis.text = element_text(size = ', fontSize,'),
  #                             axis.title.x = element_text(size = ', fontSize,'),
  #                             axis.title.y = element_text(size = ', fontSize,'),
  #                             plot.title = element_text(size = ',fontSize,')')
  
  # code <-paste0('ggplot(data,', 'aes(', x, ifelse(colorby=="None",paste0(', fill=', colourfill, ')) + '),paste0(',' ,'fill = ',colorby, ')) +')),' 
  #               geom_bar(stat="identity") +',
  #               ifelse(Theme=="NULL" | is.null(Theme),'',paste0(Theme,'+ ')),
  #               ifelse(plotTitle=='' | is.null(plotTitle),'',paste0('labs(title = ','"',plotTitle,'"',') + ')),
  #               ifelse(title_x=='' | is.null(title_x),'',paste0(' xlab(','"',title_x,'"',') + ')),
  #               ifelse(title_y=='' | is.null(title_y),'',paste0(' ylab(','"',title_y,'"',') ')),
  #               ifelse(fontSize==10 & legendPos == 'right' ,'',paste0('+ theme(axis.text = element_text(size = ', fontSize,'),
  #                                                                     axis.title.x = element_text(size = ', fontSize,'),
  #                                                                     axis.title.y = element_text(size = ', fontSize,'),
  #                                                                     plot.title = element_text(size = ',fontSize,'),
  #                                                                     legend.position = ','"',legendPos,'"',')'))
  #               )
  # 
  
  code <-HTML(paste0('<pre>ggplot(data = ',df_name, ' , aes(x=', x,  ifelse(colorby=="None",")) + <br> ",paste0(',' ,'fill = ',colorby, ')) + <br>')), 
                     paste0('geom_histogram('), ifelse(colorby=="None",paste0(', fill =',shQuote(colourfill), ') + <br>'), ') + <br>'),
                     paste0(ifelse(Theme=="NULL" | is.null(Theme),'',paste0('&ensp;',Theme,'+ <br>'))),
                     ifelse(plotTitle=='' | is.null(plotTitle),'',paste0('&ensp; labs(title = ','"',plotTitle,'"',') + <br>')),
                     ifelse(title_x=='' | is.null(title_x),'',paste0(' xlab(','"',title_x,'"',') + <br>')),
                     ifelse(title_y=='' | is.null(title_y),'',paste0(' ylab(','"',title_y,'"',') ')),
                     ifelse(fontSize==10 & legendPos == 'right' ,'',
                            paste0(paste0('+ <br> theme(axis.text = element_text(size = ', fontSize,'), <br>'),
                                   paste0(' &emsp; &emsp; &emsp; axis.title.x = element_text(size = ', fontSize,'),<br>'),
                                   paste0(' &emsp; &emsp; &emsp; axis.title.y = element_text(size = ', fontSize,'),<br>'),
                                   paste0(' &emsp; &emsp; &emsp; plot.title = element_text(size = ',fontSize,'),<br>'),
                                   paste0(' &emsp; &emsp; &emsp; legend.position = ','"',legendPos,'"',')<br>'))
                            
                     ),
                     ifelse(facetRow != 'None' & facetCol != 'None',paste0(" + <br> facet_grid(",as.formula(paste0(facetRow, "~", facetCol)),") <br>" ),''),
                     ifelse(facetRow != 'None' & facetCol == 'None',paste0('+ <br> facet_grid(',facetRow,' ~ .) <br>'),'' ),
                     ifelse(facetRow == 'None' & facetCol != 'None',paste0('+ <br> facet_grid(. ~ ',facetCol,') <br>') ,''),
                     
                     '</pre>'))
  
  
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
  
  if(hideAxis == 1){
    p <-  p + theme(axis.text.x = element_blank())
    code <- paste0(code, '+ theme(axis.text.x = element_blank())')
  }
  
  if(axisAngle > 0){
    p <-  p + theme(axis.text.x = element_text(angle = axisAngle, hjust = 1))
    code <- paste0(code, '+ theme(axis.text.x = element_text(angle = ', axisAngle, ', hjust = 1))')
  }
  
  ls <- list()
  ls[['plot']] <- p
  ls[['code']] <- code
  return(ls)
  
}