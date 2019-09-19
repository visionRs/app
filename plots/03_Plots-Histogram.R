histogram <- function(data=NULL,
                      df_name=NULL,
                      x=NULL, 
                      Theme=NULL, 
                      fontSize=10, 
                      colourfill = '#2219CCCC',
                      colorby = 'None',
                      legendPos = 'right',
                      title_x='', 
                      title_y='',
                      plotTitle='',
                      hideAxis,
                      axisAngle,
                      facetRow,
                      position = '',
                      coorflip=FALSE,
                      density=FALSE,
                      density_fill='None',
                      alpha=alpha,
                      facetCol) {
  
  print(density_fill)
  
  p <- ggplot(data, aes_string(x)) + #,fill = ifelse(colorby == 'None', shQuote("None"), colorby) )) +
    #geom_histogram(position = position) +
    {if(density) { geom_density(aes_string(fill = ifelse(density_fill == 'None', shQuote("None"), density_fill)),alpha= alpha) } 
      else { geom_histogram(aes_string(fill = ifelse(colorby == 'None', shQuote("None"), colorby)),position = position) }} +
    eval(parse(text=as.character(Theme))) +
    labs(title = plotTitle) +
    xlab(title_x) + ylab(title_y) +
    {if(coorflip) { coord_flip() } } +
    {if(colorby=='None' & density_fill=='None') { scale_fill_manual(values = colourfill) } 
      # else if(colorby=='None' & density_fill!='None')
      # { scale_fill_manual(values = colourfill) }
    } +
    {if(facetRow != 'None' & facetCol != 'None'){facet_grid(as.formula(paste0(facetRow, "~", facetCol)))} 
      else if(facetRow != 'None' & facetCol == 'None'){facet_grid(as.formula(paste0(facetRow, "~ .")))}
      else if(facetRow == 'None' & facetCol != 'None'){facet_grid(as.formula(paste0(". ~", facetCol)))}
    } +
    theme(axis.text = element_text(size = fontSize),
          axis.title.x = element_text(size = fontSize),
          axis.title.y = element_text(size = fontSize),
          plot.title = element_text(size = fontSize),
          legend.position = if (density){ if (density_fill=='None') 'none' else legendPos} else {if (colorby=='None') 'none' else legendPos},
          axis.text.x = if(hideAxis==TRUE) element_blank() else element_text(angle =axisAngle,hjust = 1)) 
  

  
  
  code <-HTML(paste0('<pre>ggplot(data = ',df_name, ' , aes(x=', x,  ifelse(colorby=="None",")) + <br> ",paste0(',' ,'fill = ',colorby, ')) + <br>')), 
                     if(density){ paste0(paste0('geom_density('), ifelse((colorby=="None" & density_fill=='None'),paste0('fill =',shQuote(colourfill), ', alpha =',alpha,') + <br>'), paste0('aes(fill=factor(',density_fill, ')), alpha =',alpha,') + <br>'))) } else { paste0(paste0('geom_histogram('), ifelse(colorby=="None",paste0('fill =',shQuote(colourfill), ') + <br>'), ') + <br>'))},
                     
                     #paste0('geom_histogram('), ifelse(colorby=="None",paste0('fill =',shQuote(colourfill), ') + <br>'), ') + <br>'),
                     paste0(ifelse(Theme=="NULL" | is.null(Theme),'',paste0('&ensp;',Theme,'+ <br>'))),
                     ifelse(plotTitle=='' | is.null(plotTitle),'',paste0('&ensp; labs(title = ','"',plotTitle,'"',') + <br>')),
                     ifelse(title_x=='' | is.null(title_x),'',paste0(' xlab(','"',title_x,'"',') + <br>')),
                     ifelse(title_y=='' | is.null(title_y),'',paste0(' ylab(','"',title_y,'"',') ')),
                     ifelse(fontSize==10 & legendPos == 'right' ,'',
                            paste0(paste0('+ <br> theme(axis.text = element_text(size = ', fontSize,'), <br>'),
                                   paste0(' &emsp; &emsp; &emsp; axis.title.x = element_text(size = ', fontSize,'),<br>'),
                                   paste0(' &emsp; &emsp; &emsp; axis.title.y = element_text(size = ', fontSize,'),<br>'),
                                   paste0(' &emsp; &emsp; &emsp; plot.title = element_text(size = ',fontSize,'),<br>'),
                                   paste0(' &emsp; &emsp; &emsp; legend.position = ','"',legendPos,'"',')<br>'),
                                   paste0(' &emsp; &emsp; &emsp; axis.text.x = ', ifelse(hideAxis, 'element_blank())', paste0('element_text(angle = ', axisAngle, ', hjust = 1))')))
                            )     
                     ),
                     ifelse(facetRow != 'None' & facetCol != 'None',paste0(" + <br> facet_grid(",as.formula(paste0(facetRow, "~", facetCol)),") <br>" ),''),
                     ifelse(facetRow != 'None' & facetCol == 'None',paste0('+ <br> facet_grid(',facetRow,' ~ .) <br>'),'' ),
                     ifelse(facetRow == 'None' & facetCol != 'None',paste0('+ <br> facet_grid(. ~ ',facetCol,') <br>') ,''),
                     ifelse(coorflip==TRUE, paste0('+ <br> coord_flip()'),paste0('')),
                     
                     '</pre>'))
  
  

  
  ls <- list()
  ls[['plot']] <- p
  ls[['code']] <- code
  return(ls)
  
}