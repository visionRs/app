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
                      lineType) {
  
  p <-  ggplot(data, aes_string(x,y, color = ifelse(colorby == 'None','NULL',colorby) ))
    #geom_line() +
  
  if(colorby == "None"){

    p <- p + geom_line(linetype = lineType, colour=colourfill) +
      eval(parse(text=as.character(Theme))) +
      labs(title = plotTitle) +
      xlab(title_x) + ylab(title_y) +
      theme(axis.text = element_text(size = fontSize),
            axis.title.x = element_text(size = fontSize),
            axis.title.y = element_text(size = fontSize),
            plot.title = element_text(size = fontSize),
            legend.position = legendPos)
  } else{
    p <- p + geom_line(linetype = lineType) +
      eval(parse(text=as.character(Theme))) +
      labs(title = plotTitle) +
      xlab(title_x) + ylab(title_y) +
      theme(axis.text = element_text(size = fontSize),
            axis.title.x = element_text(size = fontSize),
            axis.title.y = element_text(size = fontSize),
            plot.title = element_text(size = fontSize),
            legend.position = legendPos)
  }
  
  code <- paste0('ggplot(data, aes(',x,',', y, ifelse(colorby == 'None',')) +',paste0(', color =',colorby,')) + ')),
                 'geom_line(',ifelse(lineType == 'solid', paste0(') +'), paste0('linetype = ',lineType, ') +') ),
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