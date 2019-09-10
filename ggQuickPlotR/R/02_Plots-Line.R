#1 PLOTS CODE: --------------------------------

#1.1 PLOTS CODE: Line Plot---------------------
line_plot <- function(data=dt,
                      x=NULL,
                      y=NULL, 
                      Theme, 
                      colourfill, 
                      colorby, 
                      fontSize, 
                      legendPos, 
                      title_x, 
                      title_y, 
                      plotTitle, 
                      hideAxis,
                      axisAngle,
                      lineType,
                      lineSize,
                      dots,
                      facetRow,
                      facetCol) {
  
  p <-  ggplot(data, aes_string(x,y, color = ifelse(colorby == 'None','NULL',colorby) ))
  
  if(colorby == "None"){
    p <- p + geom_line(linetype = lineType, colour=colourfill, size = lineSize)
  } else{
    p <- p + geom_line(linetype = lineType, size = lineSize)
  }
  
  p <- p + eval(parse(text=as.character(Theme))) +
    labs(title = plotTitle) +
    xlab(title_x) + ylab(title_y) +
    theme(axis.text = element_text(size = fontSize),
          axis.title.x = element_text(size = fontSize),
          axis.title.y = element_text(size = fontSize),
          plot.title = element_text(size = fontSize),
          legend.position = legendPos)
  
  if(dots == TRUE){
    p <-  p + geom_point()
  }
  
  code <- paste0('ggplot(data, aes(',x,',', y, ifelse(colorby == 'None',')) +',paste0(', color =',colorby,')) + ')),
                 'geom_line(',ifelse(lineType == 'solid' & lineSize == 1, paste0(') + '), 
                                     ifelse(lineType != 'solid' & lineSize == 1, paste0('linetype = ', '"' ,lineType, '"' , ') +'),
                                            ifelse(lineType == 'solid' & lineSize != 1, paste0('size = ', lineSize, ') +'),
                                                   ifelse(lineType != 'solid' & lineSize != 1, paste0('linetype = ', '"' ,lineType, '"' ,', size = ', lineSize, ') +'))))),
                 ifelse(dots == TRUE, paste0('geom_point() + '),''),
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
  
  
  # if(Theme == 'None'){
  #   if(colorby == 'None'){
  #     p <-  ggplot(data, aes_string(x,y)) +
  #       geom_line(color = colourfill) +
  #       labs(title = plotTitle) +
  #       xlab(title_x) + ylab(title_y) +
  #       theme(axis.text = element_text(size = fontSize),
  #             axis.title.x = element_text(size = fontSize),
  #             axis.title.y = element_text(size = fontSize),
  #             plot.title = element_text(size = fontSize))
  #     
  #     code <- paste0('ggplot(data, aes(',x,',',y,')) +
  #                    geom_line(color = ',colourfill,') +
  #                    labs(title = ','"',plotTitle,'"',') +
  #                    xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
  #                    theme(axis.text = element_text(size = ', fontSize,'),
  #                    axis.title.x = element_text(size = ', fontSize,'),
  #                    axis.title.y = element_text(size = ', fontSize,'),
  #                    plot.title = element_text(size = ',fontSize,'))')
  #     ls <- list()
  #     ls[['plot']] <- p
  #     ls[['code']] <- code
  #     return(ls)
  #   } else{
  #     p <-  ggplot(data, aes_string(x,y, color = colorby)) +
  #       geom_line() +
  #       labs(title = plotTitle) +
  #       xlab(title_x) + ylab(title_y) +
  #       theme(axis.text = element_text(size = fontSize),
  #             axis.title.x = element_text(size = fontSize),
  #             axis.title.y = element_text(size = fontSize),
  #             plot.title = element_text(size = fontSize),
  #             legend.position = legendPos)
  #     
  #     code <- paste0('ggplot(data, aes(',x,',',y, ',' ,'color = ', colorby, ')) + 
  #                    geom_line() +
  #                    labs(title = ','"',plotTitle,'"',') +
  #                    xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
  #                    theme(axis.text = element_text(size = ', fontSize,'),
  #                    axis.title.x = element_text(size = ', fontSize,'),
  #                    axis.title.y = element_text(size = ', fontSize,'),
  #                    plot.title = element_text(size = ',fontSize,'),
  #                    legend.position = ','"',legendPos,'"',')')
  #     ls <- list()
  #     ls[['plot']] <- p
  #     ls[['code']] <- code
  #     return(ls)
  #   }# else ends for colorby
  # } else{
  #   if(colorby == 'None'){
  #       p <-  ggplot(data, aes_string(x,y)) +
  #         geom_line(color = colourfill) +
  #         get(Theme)() +
  #         labs(title = plotTitle) +
  #         xlab(title_x) + ylab(title_y) +
  #         theme(axis.text = element_text(size = fontSize),
  #               axis.title.x = element_text(size = fontSize),
  #               axis.title.y = element_text(size = fontSize),
  #               plot.title = element_text(size = fontSize))
  #       
  #       code <- paste0('ggplot(data, aes(',x,',',y,')) +
  #                       geom_line(color = ',colourfill,') +
  #                       ',Theme,'() +
  #                       labs(title = ','"',plotTitle,'"',') +
  #                       xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
  #                       theme(axis.text = element_text(size = ', fontSize,'),
  #                            axis.title.x = element_text(size = ', fontSize,'),
  #                            axis.title.y = element_text(size = ', fontSize,'),
  #                             plot.title = element_text(size = ',fontSize,'))')
  #       ls <- list()
  #       ls[['plot']] <- p
  #       ls[['code']] <- code
  #       return(ls)
  #   } else{
  #       p <-  ggplot(data, aes_string(x,y, color = colorby)) +
  #         geom_line() +
  #         get(Theme)() +
  #         labs(title = plotTitle) +
  #         xlab(title_x) + ylab(title_y) +
  #         theme(axis.text = element_text(size = fontSize),
  #               axis.title.x = element_text(size = fontSize),
  #               axis.title.y = element_text(size = fontSize),
  #               plot.title = element_text(size = fontSize),
  #               legend.position = legendPos)
  #       
  #       code <- paste0('ggplot(data, aes(',x,',',y, ',' ,'color = ', colorby, ')) + 
  #                       geom_line() +
  #                       ',Theme,'() +
  #                       labs(title = ','"',plotTitle,'"',') +
  #                       xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
  #                       theme(axis.text = element_text(size = ', fontSize,'),
  #                                axis.title.x = element_text(size = ', fontSize,'),
  #                                axis.title.y = element_text(size = ', fontSize,'),
  #                                 plot.title = element_text(size = ',fontSize,'),
  #                                 legend.position = ','"',legendPos,'"',')')
  #       ls <- list()
  #       ls[['plot']] <- p
  #       ls[['code']] <- code
  #       return(ls)
  #   }# else ends for colorby
  # }# else ends for Theme
}