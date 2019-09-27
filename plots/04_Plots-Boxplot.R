#1 PLOTS CODE: --------------------------------

#1.1 PLOTS CODE: Bar Plot---------------------

box_plot <- function(data=NULL,
                     df_name=NULL,
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
  
  
  p <- ggplot(data, aes_string(paste0("factor(",x,")"),y, fill = ifelse(colorby == 'None','NULL', colorby) )) +
      {if(colorby == "None"){ geom_boxplot(fill = colourfill) } else{ geom_boxplot()  }} +
      eval(parse(text=as.character(Theme))) +
      labs(title = plotTitle) +
      xlab(title_x) + ylab(title_y) +
      theme(axis.text = element_text(size = fontSize),
            axis.title.x = element_text(size = fontSize),
            axis.title.y = element_text(size = fontSize),
            plot.title = element_text(size = fontSize),
            legend.position = legendPos,
            axis.text.x = if(hideAxis==TRUE) element_blank() else element_text(angle =axisAngle,hjust = 1))+
    {if(jitter == TRUE){ geom_jitter()}} +
    {if(facetRow != 'None' & facetCol != 'None'){facet_grid(as.formula(paste0(facetRow, "~", facetCol)))} 
      else if(facetRow != 'None' & facetCol == 'None'){facet_grid(as.formula(paste0(facetRow, "~ .")))}
      else if(facetRow == 'None' & facetCol != 'None'){facet_grid(as.formula(paste0(". ~", facetCol)))}
    } 
    
  # p <-  p +
  #   eval(parse(text=as.character(Theme))) +
  #   labs(title = plotTitle) +
  #   xlab(title_x) + ylab(title_y) +
  #   theme(axis.text = element_text(size = fontSize),
  #         axis.title.x = element_text(size = fontSize),
  #         axis.title.y = element_text(size = fontSize),
  #         plot.title = element_text(size = fontSize),
  #         legend.position = legendPos)
  
  # if(jitter == TRUE){
  #   p <-  p + geom_jitter()
  # }
  
  # code <- paste0('ggplot(data,', 'aes(', x, ',', y, ifelse(colorby=="None", ')) +', paste0(',' ,'fill = ',colorby, ')) +')),
  #                 'geom_boxplot(', ifelse(colorby=='None', paste0('fill = ', '"' ,colourfill,'"',') +'),') +'),
  #                ifelse(Theme=="NULL" | is.null(Theme),'',paste0(Theme,'+ ')),
  #                ifelse(plotTitle=='' | is.null(plotTitle),'',paste0('labs(title = ','"',plotTitle,'"',') + ')),
  #                ifelse(title_x=='' | is.null(title_x),'',paste0(' xlab(','"',title_x,'"',') + ')),
  #                ifelse(title_y=='' | is.null(title_y),'',paste0(' ylab(','"',title_y,'"',') ')),
  #                ifelse(fontSize==10 & legendPos == 'right' ,'',paste0('+ theme(axis.text = element_text(size = ', fontSize,'),
  #                                                                      axis.title.x = element_text(size = ', fontSize,'),
  #                                                                      axis.title.y = element_text(size = ', fontSize,'),
  #                                                                      plot.title = element_text(size = ',fontSize,'),
  #                                                                      legend.position = ','"',legendPos,'"',')')))
  # 
  
  
  code <-HTML(paste0('<pre>ggplot(data = ',df_name, ' , aes(x=', x, ', y=', y, ifelse(colorby=="None",")) + <br> ",paste0(',' ,'fill = ',colorby, ')) + <br>')), 
                     paste0('geom_boxplot(', ifelse(colorby=="None",paste0(', fill = ',shQuote(colourfill), ') + <br>'), ') + <br>')),
                     paste0(ifelse(Theme=="NULL" | is.null(Theme),'',paste0('&ensp;',Theme,'+ <br>'))),
                     ifelse(plotTitle=='' | is.null(plotTitle),'',paste0('&ensp; labs(title = ','"',plotTitle,'"',') + <br>')),
                     ifelse(title_x=='' | is.null(title_x),'',paste0(' xlab(','"',title_x,'"',') + <br>')),
                     ifelse(title_y=='' | is.null(title_y),'',paste0(' ylab(','"',title_y,'"',') ')),
                     ifelse(fontSize==10 & legendPos == 'right' , paste0(' + <br> theme(axis.text.x = ', ifelse(hideAxis, 'element_blank())  <br>', paste0('element_text(angle = ', axisAngle, ', hjust = 1)) <br>'))),
                            paste0(paste0('+ <br> theme(axis.text = element_text(size = ', fontSize,'), <br>'),
                                   paste0(' &emsp; &emsp; &emsp; axis.title.x = element_text(size = ', fontSize,'),<br>'),
                                   paste0(' &emsp; &emsp; &emsp; axis.title.y = element_text(size = ', fontSize,'),<br>'),
                                   paste0(' &emsp; &emsp; &emsp; plot.title = element_text(size = ',fontSize,'),<br>'),
                                   paste0(' &emsp; &emsp; &emsp; legend.position = ','"',legendPos,'"',', <br>'),
                                   paste0(' &emsp; &emsp; &emsp; axis.text.x = ', ifelse(hideAxis, 'element_blank())', paste0('element_text(angle = ', axisAngle, ', hjust = 1))')))
                                   
                            )
                            
                     ),
                     ifelse(jitter , paste0('+ <br> geom_jitter() ','')),
                     ifelse(facetRow != 'None' & facetCol != 'None',paste0(" + <br> facet_grid(",as.formula(paste0(facetRow, "~", facetCol)),") " ),''),
                     ifelse(facetRow != 'None' & facetCol == 'None',paste0('+ <br> facet_grid(',facetRow,' ~ .) '),'' ),
                     ifelse(facetRow == 'None' & facetCol != 'None',paste0('+ <br> facet_grid(. ~ ',facetCol,') ') ,''),
                     ifelse(interactive == TRUE, paste0(' %>% ggplotly()'), paste0('')),
                     '</pre>'))
  
  # facet
  # if(facetRow != 'None' & facetCol != 'None'){
  #   p <-  p + facet_grid(as.formula(paste0(facetRow, "~", facetCol)))
  #   code <- paste0(code,'+ facet_grid(',facetRow,' ~ ',facetCol,')')
  # }
  # if(facetRow != 'None' & facetCol == 'None'){
  #   p <-  p + facet_grid(as.formula(paste0(facetRow, "~ .")))
  #   code <- paste0(code,'+ facet_grid(',facetRow,' ~ .)')
  # }
  # if(facetRow == 'None' & facetCol != 'None'){
  #   p <-  p + facet_grid(as.formula(paste0(". ~", facetCol)))
  #   code <- paste0(code,'+ facet_grid(. ~ ',facetCol,')')
  # }
  


  
  
  ls <- list()
  ls[['plot']] <- p
  ls[['code']] <- code
  return(ls)
}