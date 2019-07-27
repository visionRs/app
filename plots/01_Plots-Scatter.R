#1 PLOTS CODE: --------------------------------


#1.1 PLOTS CODE: Scatter Plot---------------------
scatter_plot <- function(data=dt,x=NULL,y=NULL, Theme, colourfill, colorby, fontSize, legendPos, dotSize, dotOpa, title_x, title_y, plotTitle,
                         regressionLine, correlation) {
  if(colorby == 'None'){
      if(regressionLine == TRUE){
        if(correlation == TRUE){
          p <- ggplot(data, aes_string(x,y)) +
            geom_point(size = dotSize, alpha = dotOpa, colour = colourfill) +
            geom_smooth(method=lm, se=FALSE) +
            get(Theme)() +
            labs(title = plotTitle) +
            xlab(title_x) + ylab(title_y) +
            theme(axis.text = element_text(size = fontSize),
                  axis.title.x = element_text(size = fontSize),
                  axis.title.y = element_text(size = fontSize),
                  plot.title = element_text(size = fontSize)) +
            stat_cor(method = "pearson")
          
          code <- paste0('ggplot(data, aes(',x,',',y,')) +
                         geom_point(size = ',dotSize,', alpha = ',dotOpa,', colour = ','"',colourfill,'"',') +
                          geom_smooth(method=lm, se=FALSE) +
                         ',Theme,'() +
                          labs(title = ','"',plotTitle,'"',') +
                         xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
                         theme(axis.text = element_text(size = ', fontSize,'),
                                axis.title.x = element_text(size = ', fontSize,'),
                                axis.title.y = element_text(size = ', fontSize,'),
                                plot.title = element_text(size = ',fontSize,')) +
                          stat_cor(method = "pearson")')
          ls <- list()
          ls[['plot']] <- p
          ls[['code']] <- code
          return(ls)
          
        } else{
        p <- ggplot(data, aes_string(x,y)) +
          geom_point(size = dotSize, alpha = dotOpa, colour = colourfill) +
          geom_smooth(method=lm, se=FALSE) +
          get(Theme)() +
          labs(title = plotTitle) +
          xlab(title_x) + ylab(title_y) +
          theme(axis.text = element_text(size = fontSize),
                axis.title.x = element_text(size = fontSize),
                axis.title.y = element_text(size = fontSize),
                plot.title = element_text(size = fontSize))
        
        code <- paste0('ggplot(data, aes(',x,',',y,')) +
                       geom_point(size = ',dotSize,', alpha = ',dotOpa,', colour = ','"',colourfill,'"',') +
                        geom_smooth(method=lm, se=FALSE) +
                        ',Theme,'() +
                       labs(title = ','"',plotTitle,'"',') +
                       xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
                       theme(axis.text = element_text(size = ', fontSize,'),
                             axis.title.x = element_text(size = ', fontSize,'),
                             axis.title.y = element_text(size = ', fontSize,'),
                             plot.title = element_text(size = ',fontSize,'))')
        ls <- list()
        ls[['plot']] <- p
        ls[['code']] <- code
        return(ls)
        }# else ends here for correlation condition
        
      } else{ # no regression line
      p <- ggplot(data, aes_string(x,y)) +
        geom_point(size = dotSize, alpha = dotOpa, colour = colourfill) +
        get(Theme)() +
        labs(title = plotTitle) +
        xlab(title_x) + ylab(title_y) +
        theme(axis.text = element_text(size = fontSize),
              axis.title.x = element_text(size = fontSize),
              axis.title.y = element_text(size = fontSize),
              plot.title = element_text(size = fontSize))
      
      code <- paste0('ggplot(data, aes(',x,',',y,')) +
                      geom_point(size = ',dotSize,', alpha = ',dotOpa,', colour = ','"',colourfill,'"',') +
                      ',Theme,'() +
                      labs(title = ','"',plotTitle,'"',') +
                      xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
                      theme(axis.text = element_text(size = ', fontSize,'),
                               axis.title.x = element_text(size = ', fontSize,'),
                               axis.title.y = element_text(size = ', fontSize,'),
                                plot.title = element_text(size = ',fontSize,'))')
      ls <- list()
      ls[['plot']] <- p
      ls[['code']] <- code
      return(ls)
      } # else ends for regressionLine condition
  } else{
    if(regressionLine == TRUE){
      if(correlation == TRUE){
        p <- ggplot(data, aes_string(x,y, color = colorby)) +
          geom_point(size = dotSize, alpha = dotOpa) +
          geom_smooth(method=lm, se=FALSE) +
          get(Theme)() +
          labs(title = plotTitle) +
          xlab(title_x) + ylab(title_y) +
          theme(axis.text = element_text(size = fontSize),
                axis.title.x = element_text(size = fontSize),
                axis.title.y = element_text(size = fontSize),
                plot.title = element_text(size = fontSize),
                legend.position = legendPos) +
          stat_cor(method = "pearson")
        
        code <- paste0('ggplot(data, aes(',x,',',y, ',' ,'color = ', colorby, ')) + 
                       geom_point(size = ',dotSize,', alpha = ',dotOpa,') +
                        geom_smooth(method=lm, se=FALSE) +
                        ',Theme,'() +
                       labs(title = ','"',plotTitle,'"',') +
                       xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
                       theme(axis.text = element_text(size = ', fontSize,'),
                             axis.title.x = element_text(size = ', fontSize,'),
                             axis.title.y = element_text(size = ', fontSize,'),
                             plot.title = element_text(size = ',fontSize,'),
                             legend.position = ','"',legendPos,'"',') +
                        stat_cor(method = "pearson")')
        ls <- list()
        ls[['plot']] <- p
        ls[['code']] <- code
        return(ls)
      } else{
      p <- ggplot(data, aes_string(x,y, color = colorby)) +
        geom_point(size = dotSize, alpha = dotOpa) +
        geom_smooth(method=lm, se=FALSE) +
        get(Theme)() +
        labs(title = plotTitle) +
        xlab(title_x) + ylab(title_y) +
        theme(axis.text = element_text(size = fontSize),
              axis.title.x = element_text(size = fontSize),
              axis.title.y = element_text(size = fontSize),
              plot.title = element_text(size = fontSize),
              legend.position = legendPos)
      
      code <- paste0('ggplot(data, aes(',x,',',y, ',' ,'color = ', colorby, ')) + 
                     geom_point(size = ',dotSize,', alpha = ',dotOpa,') +
                      geom_smooth(method=lm, se=FALSE) +
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
      } # else ends for correlation condition
    } else{ # no regression line
      p <- ggplot(data, aes_string(x,y, color = colorby)) +
        geom_point(size = dotSize, alpha = dotOpa) +
        get(Theme)() +
        labs(title = plotTitle) +
        xlab(title_x) + ylab(title_y) +
        theme(axis.text = element_text(size = fontSize),
              axis.title.x = element_text(size = fontSize),
              axis.title.y = element_text(size = fontSize),
              plot.title = element_text(size = fontSize),
              legend.position = legendPos)
      
      code <- paste0('ggplot(data, aes(',x,',',y, ',' ,'color = ', colorby, ')) + 
                      geom_point(size = ',dotSize,', alpha = ',dotOpa,') +
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
    }# else ends for regressionLine condition
  } # else ends for colorby condition
}