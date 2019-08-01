#' @title Bar Plot Code
#' @description Helper function to generate bar plot based on different conditions.
#' @param data data.frame - Passed from shiny enviornment
#' @param x character- This is x variable for aesthitic, supplied from shiny enviornment.
#' @param y character- This is y variable for aesthitic, supplied from shiny enviornment.
#' @param colorby character - Parameter used to colorby plot. (defaults to None).
#' @param colourfill character - Parameter used to colour plot.
#' @param Theme character- parameter to change plot theme. (defaults to None).
#' @param fontsize numeric- This parameter controls fontsize for x-y titles, x-y ticks labels. (defaults to 10).
#' @param legendPos character- Adjust Legend position (defaults to right)
#' @param title_x character- Set x axis title.
#' @param title_y character- Set y axis title.
#' @param plotTitle character- Set plot title.
#' @param dotSize numeric- Set size of dots.
#' @param dotOpa numeric- Set opacity of dots.
#' @param regressionLine checkbox- to add regression line to plot.
#' @param correlation checkbox- to calculate pearson correlation.
#' @return returns H2OFrame
#' @export


#1 PLOTS CODE: --------------------------------


#1.1 PLOTS CODE: Scatter Plot---------------------
scatter_plot <- function(data=dt,
                         x=NULL,
                         y=NULL, 
                         Theme=NULL, 
                         colourfill='#00FF0080', 
                         colorby='None', 
                         fontSize=10, 
                         legendPos='right', 
                         dotSize=2, 
                         dotOpa=0.7, 
                         title_x='', 
                         title_y ='', 
                         plotTitle ='',
                         regressionLine=FALSE, 
                         correlation=FALSE) {
  

  
  p <- ggplot(data, aes_string(x,y, color = ifelse(colorby == 'None','NULL',colorby) ))
  if(colorby == "None"){
    
    p <- p + geom_point(size = dotSize, alpha = dotOpa, color=colourfill) +
      eval(parse(text=as.character(Theme))) +
      labs(title = plotTitle) +
      xlab(title_x) + ylab(title_y) +
      theme(axis.text = element_text(size = fontSize),
            axis.title.x = element_text(size = fontSize),
            axis.title.y = element_text(size = fontSize),
            plot.title = element_text(size = fontSize),
            legend.position = legendPos)
  } else{
    p <- p + geom_point(size = dotSize, alpha = dotOpa) +
      eval(parse(text=as.character(Theme))) +
      labs(title = plotTitle) +
      xlab(title_x) + ylab(title_y) +
      theme(axis.text = element_text(size = fontSize),
            axis.title.x = element_text(size = fontSize),
            axis.title.y = element_text(size = fontSize),
            plot.title = element_text(size = fontSize),
            legend.position = legendPos)
  }

  code <- paste0('ggplot(data,', 'aes(', x, ',', y, ifelse(colorby=='None',')) +', paste0(',' ,'color = ',colorby, ')) +')),' 
                  geom_point(size = ',dotSize, ',alpha = ',dotOpa, ifelse(colorby=='NULL', paste0(', colour = ', '"' ,colourfill,'"',') +'),' ) +'),
                 ifelse(Theme=="NULL" | is.null(Theme),'',paste0(Theme,'+ ')),
                 ifelse(plotTitle=='' | is.null(plotTitle),'',paste0('labs(title = ','"',plotTitle,'"',') + ')),
                 ifelse(title_x=='' | is.null(title_x),'',paste0(' xlab(','"',title_x,'"',') + ')),
                 ifelse(title_y=='' | is.null(title_y),'',paste0(' ylab(','"',title_y,'"',') ')),
                 ifelse(fontSize==10 & legendPos == 'right' ,'',paste0('+ theme(axis.text = element_text(size = ', fontSize,'),
                                                                       axis.title.x = element_text(size = ', fontSize,'),
                                                                       axis.title.y = element_text(size = ', fontSize,'),
                                                                       plot.title = element_text(size = ',fontSize,'),
                                                                       legend.position = ','"',legendPos,'"',')')))
  
  if(regressionLine == TRUE & correlation == TRUE){
    p <- p + geom_smooth(method=lm, se=FALSE) + stat_cor(method = "pearson")
    
    code <-  paste0(code, '+ geom_smooth(method=lm, se=FALSE) + stat_cor(method = "pearson")')
  } else if(regressionLine == TRUE & correlation == "NULL") {
    p <- p + geom_smooth(method=lm, se=FALSE)
    code <-  paste0(code, '+ geom_smooth(method=lm, se=FALSE)')
  }
  
  
  ls <- list()
  ls[['plot']] <- p
  ls[['code']] <- code
  return(ls)
  
}  
  
  # if(Theme == 'None'){
  #   if(colorby == 'None'){
  #     if(regressionLine == TRUE){
  #       if(correlation == TRUE){
  #         p <- ggplot(data, aes_string(x,y)) +
  #           geom_point(size = dotSize, alpha = dotOpa, colour = colourfill) +
  #           geom_smooth(method=lm, se=FALSE) +
  #           labs(title = plotTitle) +
  #           xlab(title_x) + ylab(title_y) +
  #           theme(axis.text = element_text(size = fontSize),
  #                 axis.title.x = element_text(size = fontSize),
  #                 axis.title.y = element_text(size = fontSize),
  #                 plot.title = element_text(size = fontSize)) +
  #           stat_cor(method = "pearson")
  #         
  #         code <- paste0('ggplot(data, aes(',x,',',y,')) +
  #                        geom_point(size = ',dotSize,', alpha = ',dotOpa,', colour = ','"',colourfill,'"',') +
  #                        geom_smooth(method=lm, se=FALSE) +
  #                        labs(title = ','"',plotTitle,'"',') +
  #                        xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
  #                        theme(axis.text = element_text(size = ', fontSize,'),
  #                        axis.title.x = element_text(size = ', fontSize,'),
  #                        axis.title.y = element_text(size = ', fontSize,'),
  #                        plot.title = element_text(size = ',fontSize,')) +
  #                        stat_cor(method = "pearson")')
  #         ls <- list()
  #         ls[['plot']] <- p
  #         ls[['code']] <- code
  #         return(ls)
  #         
  #       } else{
  #         p <- ggplot(data, aes_string(x,y)) +
  #           geom_point(size = dotSize, alpha = dotOpa, colour = colourfill) +
  #           geom_smooth(method=lm, se=FALSE) +
  #           labs(title = plotTitle) +
  #           xlab(title_x) + ylab(title_y) +
  #           theme(axis.text = element_text(size = fontSize),
  #                 axis.title.x = element_text(size = fontSize),
  #                 axis.title.y = element_text(size = fontSize),
  #                 plot.title = element_text(size = fontSize))
  #         
  #         code <- paste0('ggplot(data, aes(',x,',',y,')) +
  #                        geom_point(size = ',dotSize,', alpha = ',dotOpa,', colour = ','"',colourfill,'"',') +
  #                        geom_smooth(method=lm, se=FALSE) +
  #                        labs(title = ','"',plotTitle,'"',') +
  #                        xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
  #                        theme(axis.text = element_text(size = ', fontSize,'),
  #                        axis.title.x = element_text(size = ', fontSize,'),
  #                        axis.title.y = element_text(size = ', fontSize,'),
  #                        plot.title = element_text(size = ',fontSize,'))')
  #         ls <- list()
  #         ls[['plot']] <- p
  #         ls[['code']] <- code
  #         return(ls)
  #       }# else ends here for correlation condition
  #       
  #     } else{ # no regression line
  #       p <- ggplot(data, aes_string(x,y)) +
  #         geom_point(size = dotSize, alpha = dotOpa, colour = colourfill) +
  #         labs(title = plotTitle) +
  #         xlab(title_x) + ylab(title_y) +
  #         theme(axis.text = element_text(size = fontSize),
  #               axis.title.x = element_text(size = fontSize),
  #               axis.title.y = element_text(size = fontSize),
  #               plot.title = element_text(size = fontSize))
  #       
  #       code <- paste0('ggplot(data, aes(',x,',',y,')) +
  #                      geom_point(size = ',dotSize,', alpha = ',dotOpa,', colour = ','"',colourfill,'"',') +
  #                      labs(title = ','"',plotTitle,'"',') +
  #                      xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
  #                      theme(axis.text = element_text(size = ', fontSize,'),
  #                      axis.title.x = element_text(size = ', fontSize,'),
  #                      axis.title.y = element_text(size = ', fontSize,'),
  #                      plot.title = element_text(size = ',fontSize,'))')
  #       ls <- list()
  #       ls[['plot']] <- p
  #       ls[['code']] <- code
  #       return(ls)
  #     } # else ends for regressionLine condition
  #   } else{
  #     if(regressionLine == TRUE){
  #       if(correlation == TRUE){
  #         p <- ggplot(data, aes_string(x,y, color = colorby)) +
  #           geom_point(size = dotSize, alpha = dotOpa) +
  #           geom_smooth(method=lm, se=FALSE) +
  #           labs(title = plotTitle) +
  #           xlab(title_x) + ylab(title_y) +
  #           theme(axis.text = element_text(size = fontSize),
  #                 axis.title.x = element_text(size = fontSize),
  #                 axis.title.y = element_text(size = fontSize),
  #                 plot.title = element_text(size = fontSize),
  #                 legend.position = legendPos) +
  #           stat_cor(method = "pearson")
  #         
  #         code <- paste0('ggplot(data, aes(',x,',',y, ',' ,'color = ', colorby, ')) + 
  #                        geom_point(size = ',dotSize,', alpha = ',dotOpa,') +
  #                        geom_smooth(method=lm, se=FALSE) +
  #                        labs(title = ','"',plotTitle,'"',') +
  #                        xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
  #                        theme(axis.text = element_text(size = ', fontSize,'),
  #                        axis.title.x = element_text(size = ', fontSize,'),
  #                        axis.title.y = element_text(size = ', fontSize,'),
  #                        plot.title = element_text(size = ',fontSize,'),
  #                        legend.position = ','"',legendPos,'"',') +
  #                        stat_cor(method = "pearson")')
  #         ls <- list()
  #         ls[['plot']] <- p
  #         ls[['code']] <- code
  #         return(ls)
  #       } else{
  #         p <- ggplot(data, aes_string(x,y, color = colorby)) +
  #           geom_point(size = dotSize, alpha = dotOpa) +
  #           geom_smooth(method=lm, se=FALSE) +
  #           labs(title = plotTitle) +
  #           xlab(title_x) + ylab(title_y) +
  #           theme(axis.text = element_text(size = fontSize),
  #                 axis.title.x = element_text(size = fontSize),
  #                 axis.title.y = element_text(size = fontSize),
  #                 plot.title = element_text(size = fontSize),
  #                 legend.position = legendPos)
  #         
  #         code <- paste0('ggplot(data, aes(',x,',',y, ',' ,'color = ', colorby, ')) + 
  #                        geom_point(size = ',dotSize,', alpha = ',dotOpa,') +
  #                        geom_smooth(method=lm, se=FALSE) +
  #                        labs(title = ','"',plotTitle,'"',') +
  #                        xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
  #                        theme(axis.text = element_text(size = ', fontSize,'),
  #                        axis.title.x = element_text(size = ', fontSize,'),
  #                        axis.title.y = element_text(size = ', fontSize,'),
  #                        plot.title = element_text(size = ',fontSize,'),
  #                        legend.position = ','"',legendPos,'"',')')
  #         ls <- list()
  #         ls[['plot']] <- p
  #         ls[['code']] <- code
  #         return(ls)
  #       } # else ends for correlation condition
  #     } else{ # no regression line
  #       p <- ggplot(data, aes_string(x,y, color = colorby)) +
  #         geom_point(size = dotSize, alpha = dotOpa) +
  #         labs(title = plotTitle) +
  #         xlab(title_x) + ylab(title_y) +
  #         theme(axis.text = element_text(size = fontSize),
  #               axis.title.x = element_text(size = fontSize),
  #               axis.title.y = element_text(size = fontSize),
  #               plot.title = element_text(size = fontSize),
  #               legend.position = legendPos)
  #       
  #       code <- paste0('ggplot(data, aes(',x,',',y, ',' ,'color = ', colorby, ')) + 
  #                      geom_point(size = ',dotSize,', alpha = ',dotOpa,') +
  #                      labs(title = ','"',plotTitle,'"',') +
  #                      xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
  #                      theme(axis.text = element_text(size = ', fontSize,'),
  #                      axis.title.x = element_text(size = ', fontSize,'),
  #                      axis.title.y = element_text(size = ', fontSize,'),
  #                      plot.title = element_text(size = ',fontSize,'),
  #                      legend.position = ','"',legendPos,'"',')')
  #       ls <- list()
  #       ls[['plot']] <- p
  #       ls[['code']] <- code
  #       return(ls)
  #     }# else ends for regressionLine condition
  #   } # else ends for colorby condition
  #   
  # } else{
  #     if(colorby == 'None'){
  #         if(regressionLine == TRUE){
  #           if(correlation == TRUE){
  #             p <- ggplot(data, aes_string(x,y)) +
  #               geom_point(size = dotSize, alpha = dotOpa, colour = colourfill) +
  #               geom_smooth(method=lm, se=FALSE) +
  #               get(Theme)() +
  #               labs(title = plotTitle) +
  #               xlab(title_x) + ylab(title_y) +
  #               theme(axis.text = element_text(size = fontSize),
  #                     axis.title.x = element_text(size = fontSize),
  #                     axis.title.y = element_text(size = fontSize),
  #                     plot.title = element_text(size = fontSize)) +
  #               stat_cor(method = "pearson")
  #             
  #             code <- paste0('ggplot(data, aes(',x,',',y,')) +
  #                            geom_point(size = ',dotSize,', alpha = ',dotOpa,', colour = ','"',colourfill,'"',') +
  #                             geom_smooth(method=lm, se=FALSE) +
  #                            ',Theme,'() +
  #                             labs(title = ','"',plotTitle,'"',') +
  #                            xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
  #                            theme(axis.text = element_text(size = ', fontSize,'),
  #                                   axis.title.x = element_text(size = ', fontSize,'),
  #                                   axis.title.y = element_text(size = ', fontSize,'),
  #                                   plot.title = element_text(size = ',fontSize,')) +
  #                             stat_cor(method = "pearson")')
  #             ls <- list()
  #             ls[['plot']] <- p
  #             ls[['code']] <- code
  #             return(ls)
  #             
  #           } else{
  #           p <- ggplot(data, aes_string(x,y)) +
  #             geom_point(size = dotSize, alpha = dotOpa, colour = colourfill) +
  #             geom_smooth(method=lm, se=FALSE) +
  #             get(Theme)() +
  #             labs(title = plotTitle) +
  #             xlab(title_x) + ylab(title_y) +
  #             theme(axis.text = element_text(size = fontSize),
  #                   axis.title.x = element_text(size = fontSize),
  #                   axis.title.y = element_text(size = fontSize),
  #                   plot.title = element_text(size = fontSize))
  #           
  #           code <- paste0('ggplot(data, aes(',x,',',y,')) +
  #                          geom_point(size = ',dotSize,', alpha = ',dotOpa,', colour = ','"',colourfill,'"',') +
  #                           geom_smooth(method=lm, se=FALSE) +
  #                           ',Theme,'() +
  #                          labs(title = ','"',plotTitle,'"',') +
  #                          xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
  #                          theme(axis.text = element_text(size = ', fontSize,'),
  #                                axis.title.x = element_text(size = ', fontSize,'),
  #                                axis.title.y = element_text(size = ', fontSize,'),
  #                                plot.title = element_text(size = ',fontSize,'))')
  #           ls <- list()
  #           ls[['plot']] <- p
  #           ls[['code']] <- code
  #           return(ls)
  #           }# else ends here for correlation condition
  #           
  #         } else{ # no regression line
  #         p <- ggplot(data, aes_string(x,y)) +
  #           geom_point(size = dotSize, alpha = dotOpa, colour = colourfill) +
  #           get(Theme)() +
  #           labs(title = plotTitle) +
  #           xlab(title_x) + ylab(title_y) +
  #           theme(axis.text = element_text(size = fontSize),
  #                 axis.title.x = element_text(size = fontSize),
  #                 axis.title.y = element_text(size = fontSize),
  #                 plot.title = element_text(size = fontSize))
  #         
  #         code <- paste0('ggplot(data, aes(',x,',',y,')) +
  #                         geom_point(size = ',dotSize,', alpha = ',dotOpa,', colour = ','"',colourfill,'"',') +
  #                         ',Theme,'() +
  #                         labs(title = ','"',plotTitle,'"',') +
  #                         xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
  #                         theme(axis.text = element_text(size = ', fontSize,'),
  #                                  axis.title.x = element_text(size = ', fontSize,'),
  #                                  axis.title.y = element_text(size = ', fontSize,'),
  #                                   plot.title = element_text(size = ',fontSize,'))')
  #         ls <- list()
  #         ls[['plot']] <- p
  #         ls[['code']] <- code
  #         return(ls)
  #         } # else ends for regressionLine condition
  #     } else{
  #       if(regressionLine == TRUE){
  #         if(correlation == TRUE){
  #           p <- ggplot(data, aes_string(x,y, color = colorby)) +
  #             geom_point(size = dotSize, alpha = dotOpa) +
  #             geom_smooth(method=lm, se=FALSE) +
  #             get(Theme)() +
  #             labs(title = plotTitle) +
  #             xlab(title_x) + ylab(title_y) +
  #             theme(axis.text = element_text(size = fontSize),
  #                   axis.title.x = element_text(size = fontSize),
  #                   axis.title.y = element_text(size = fontSize),
  #                   plot.title = element_text(size = fontSize),
  #                   legend.position = legendPos) +
  #             stat_cor(method = "pearson")
  #           
  #           code <- paste0('ggplot(data, aes(',x,',',y, ',' ,'color = ', colorby, ')) + 
  #                          geom_point(size = ',dotSize,', alpha = ',dotOpa,') +
  #                           geom_smooth(method=lm, se=FALSE) +
  #                           ',Theme,'() +
  #                          labs(title = ','"',plotTitle,'"',') +
  #                          xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
  #                          theme(axis.text = element_text(size = ', fontSize,'),
  #                                axis.title.x = element_text(size = ', fontSize,'),
  #                                axis.title.y = element_text(size = ', fontSize,'),
  #                                plot.title = element_text(size = ',fontSize,'),
  #                                legend.position = ','"',legendPos,'"',') +
  #                           stat_cor(method = "pearson")')
  #           ls <- list()
  #           ls[['plot']] <- p
  #           ls[['code']] <- code
  #           return(ls)
  #         } else{
  #         p <- ggplot(data, aes_string(x,y, color = colorby)) +
  #           geom_point(size = dotSize, alpha = dotOpa) +
  #           geom_smooth(method=lm, se=FALSE) +
  #           get(Theme)() +
  #           labs(title = plotTitle) +
  #           xlab(title_x) + ylab(title_y) +
  #           theme(axis.text = element_text(size = fontSize),
  #                 axis.title.x = element_text(size = fontSize),
  #                 axis.title.y = element_text(size = fontSize),
  #                 plot.title = element_text(size = fontSize),
  #                 legend.position = legendPos)
  #         
  #         code <- paste0('ggplot(data, aes(',x,',',y, ',' ,'color = ', colorby, ')) + 
  #                        geom_point(size = ',dotSize,', alpha = ',dotOpa,') +
  #                         geom_smooth(method=lm, se=FALSE) +
  #                         ',Theme,'() +
  #                        labs(title = ','"',plotTitle,'"',') +
  #                        xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
  #                        theme(axis.text = element_text(size = ', fontSize,'),
  #                              axis.title.x = element_text(size = ', fontSize,'),
  #                              axis.title.y = element_text(size = ', fontSize,'),
  #                              plot.title = element_text(size = ',fontSize,'),
  #                              legend.position = ','"',legendPos,'"',')')
  #         ls <- list()
  #         ls[['plot']] <- p
  #         ls[['code']] <- code
  #         return(ls)
  #         } # else ends for correlation condition
  #       } else{ # no regression line
  #         p <- ggplot(data, aes_string(x,y, color = colorby)) +
  #           geom_point(size = dotSize, alpha = dotOpa) +
  #           get(Theme)() +
  #           labs(title = plotTitle) +
  #           xlab(title_x) + ylab(title_y) +
  #           theme(axis.text = element_text(size = fontSize),
  #                 axis.title.x = element_text(size = fontSize),
  #                 axis.title.y = element_text(size = fontSize),
  #                 plot.title = element_text(size = fontSize),
  #                 legend.position = legendPos)
  #         
  #         code <- paste0('ggplot(data, aes(',x,',',y, ',' ,'color = ', colorby, ')) + 
  #                         geom_point(size = ',dotSize,', alpha = ',dotOpa,') +
  #                         ',Theme,'() +
  #                         labs(title = ','"',plotTitle,'"',') +
  #                         xlab(','"',title_x,'"',') + ylab(','"',title_y,'"',') +
  #                         theme(axis.text = element_text(size = ', fontSize,'),
  #                                  axis.title.x = element_text(size = ', fontSize,'),
  #                                  axis.title.y = element_text(size = ', fontSize,'),
  #                                   plot.title = element_text(size = ',fontSize,'),
  #                                   legend.position = ','"',legendPos,'"',')')
  #         ls <- list()
  #         ls[['plot']] <- p
  #         ls[['code']] <- code
  #         return(ls)
  #       }# else ends for regressionLine condition
  #     } # else ends for colorby condition
  # } # else ends for Theme

