#' @title Line Plot Code
#' @description Helper function to generate Line plot based on different conditions.
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
#' @param facetRow character- facet column name, if faceted by Row
#' @param facetCol character- facet column name, if faceted by Column
#' @param hideAxis Logical - If we want to hide x Axis labels. (default: FALSE)
#' @param axisAngle Numeric- change x tick labels angle. (default: 90)
#' @return returns H2OFrame
#' @export



#1 PLOTS CODE: --------------------------------

#1.1 PLOTS CODE: Line Plot---------------------
# line_plot <- function(data=dt,
#                       x=NULL,
#                       y=NULL, 
#                       Theme, 
#                       colourfill, 
#                       colorby, 
#                       fontSize, 
#                       legendPos, 
#                       title_x, 
#                       title_y, 
#                       plotTitle, 
#                       hideAxis,
#                       axisAngle,
#                       lineType,
#                       lineSize,
#                       dots,
#                       facetRow,
#                       facetCol) {
  
#   p <-  ggplot(data, aes_string(x,y, color = ifelse(colorby == 'None','NULL',colorby) ))
#   
#   if(colorby == "None"){
#     p <- p + geom_line(linetype = lineType, colour=colourfill, size = lineSize)
#   } else{
#     p <- p + geom_line(linetype = lineType, size = lineSize)
#   }
#   
#   p <- p + eval(parse(text=as.character(Theme))) +
#     labs(title = plotTitle) +
#     xlab(title_x) + ylab(title_y) +
#     theme(axis.text = element_text(size = fontSize),
#           axis.title.x = element_text(size = fontSize),
#           axis.title.y = element_text(size = fontSize),
#           plot.title = element_text(size = fontSize),
#           legend.position = legendPos)
#   
#   if(dots == TRUE){
#     p <-  p + geom_point()
#   }
#   
#   code <- paste0('ggplot(data, aes(',x,',', y, ifelse(colorby == 'None',')) +',paste0(', color =',colorby,')) + ')),
#                  'geom_line(',ifelse(lineType == 'solid' & lineSize == 1, paste0(') + '), 
#                                      ifelse(lineType != 'solid' & lineSize == 1, paste0('linetype = ', '"' ,lineType, '"' , ') +'),
#                                             ifelse(lineType == 'solid' & lineSize != 1, paste0('size = ', lineSize, ') +'),
#                                                    ifelse(lineType != 'solid' & lineSize != 1, paste0('linetype = ', '"' ,lineType, '"' ,', size = ', lineSize, ') +'))))),
#                  ifelse(dots == TRUE, paste0('geom_point() + '),''),
#                  ifelse(Theme=="NULL" | is.null(Theme),'',paste0(Theme,'+ ')),
#                  ifelse(plotTitle=='' | is.null(plotTitle),'',paste0('labs(title = ','"',plotTitle,'"',') + ')),
#                  ifelse(title_x=='' | is.null(title_x),'',paste0(' xlab(','"',title_x,'"',') + ')),
#                  ifelse(title_y=='' | is.null(title_y),'',paste0(' ylab(','"',title_y,'"',') ')),
#                  ifelse(fontSize==10 & legendPos == 'right' ,'',paste0('+ theme(axis.text = element_text(size = ', fontSize,'),
#                                                                        axis.title.x = element_text(size = ', fontSize,'),
#                                                                        axis.title.y = element_text(size = ', fontSize,'),
#                                                                        plot.title = element_text(size = ',fontSize,'),
#                                                                        legend.position = ','"',legendPos,'"',')')))
#   
#   
#   # facet
#   if(facetRow != 'None' & facetCol != 'None'){
#     p <-  p + facet_grid(as.formula(paste0(facetRow, "~", facetCol)))
#     code <- paste0(code,'+ facet_grid(',facetRow,' ~ ',facetCol,')')
#   }
#   if(facetRow != 'None' & facetCol == 'None'){
#     p <-  p + facet_grid(as.formula(paste0(facetRow, "~ .")))
#     code <- paste0(code,'+ facet_grid(',facetRow,' ~ .)')
#   }
#   if(facetRow == 'None' & facetCol != 'None'){
#     p <-  p + facet_grid(as.formula(paste0(". ~", facetCol)))
#     code <- paste0(code,'+ facet_grid(. ~ ',facetCol,')')
#   }
#   
#   if(hideAxis == TRUE){
#     p <-  p + theme(axis.text.x = element_blank())
#     code <- paste0(code, '+ theme(axis.text.x = element_blank())')
#   } else{
#     if(axisAngle > 0){
#       p <-  p + theme(axis.text.x = element_text(angle = axisAngle, hjust = 1))
#       code <- paste0(code, '+ theme(axis.text.x = element_text(angle = ', axisAngle, ', hjust = 1))')
#     }
#   }
#   
#   ls <- list()
#   ls[['plot']] <- p
#   ls[['code']] <- code
#   return(ls)
#   
#   
# 
# }
  
  
  #1 PLOTS CODE: --------------------------------
  
  #1.1 PLOTS CODE: Line Plot---------------------
  line_plot <- function(data=dt,
                        df_name=NULL,
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
    
    # p <-  ggplot(data, aes_string(x,y, color = ifelse(colorby == 'None','NULL',colorby) ))
    #
    # if(colorby == "None"){
    #   p <- p + geom_line(linetype = lineType, colour=colourfill, size = lineSize)
    # } else{
    #   p <- p + geom_line(linetype = lineType, size = lineSize)
    # }
    
    # p <- p + eval(parse(text=as.character(Theme))) +
    #   labs(title = plotTitle) +
    #   xlab(title_x) + ylab(title_y) +
    #   theme(axis.text = element_text(size = fontSize),
    #         axis.title.x = element_text(size = fontSize),
    #         axis.title.y = element_text(size = fontSize),
    #         plot.title = element_text(size = fontSize),
    #         legend.position = legendPos)
    
    # if(dots == TRUE){
    #   p <-  p + geom_point()
    # }
    
    
    p <- ggplot(data,
                aes_string(x, y, color = ifelse(colorby == 'None','NULL',colorby))) +
      {if(colorby == "None") geom_line(linetype = lineType, colour=colourfill, size = lineSize) else geom_line(linetype = lineType, size = lineSize)} +
      {if(dots) geom_point()} +
      eval(parse(text=as.character(Theme))) +
      labs(title = plotTitle) +
      xlab(title_x) + ylab(title_y) +
      theme(axis.text = element_text(size = fontSize),
            axis.title.x = element_text(size = fontSize),
            axis.title.y = element_text(size = fontSize),
            plot.title = element_text(size = fontSize),
            legend.position = legendPos,
            axis.text.x = if(hideAxis==TRUE) element_blank() else element_text(angle =axisAngle,hjust = 1))
    
    
    # code <- paste0('ggplot(data, aes(',x,',', y, ifelse(colorby == 'None',')) +',paste0(', color =',colorby,')) + ')),
    #                'geom_line(',ifelse(lineType == 'solid' & lineSize == 1, paste0(') + '),
    #                                    ifelse(lineType != 'solid' & lineSize == 1, paste0('linetype = ', '"' ,lineType, '"' , ') +'),
    #                                           ifelse(lineType == 'solid' & lineSize != 1, paste0('size = ', lineSize, ') +'),
    #                                                  ifelse(lineType != 'solid' & lineSize != 1, paste0('linetype = ', '"' ,lineType, '"' ,', size = ', lineSize, ') +'))))),
    #                ifelse(dots == TRUE, paste0('geom_point() + '),''),
    #                ifelse(Theme=="NULL" | is.null(Theme),'',paste0(Theme,'+ ')),
    #                ifelse(plotTitle=='' | is.null(plotTitle),'',paste0('labs(title = ','"',plotTitle,'"',') + ')),
    #                ifelse(title_x=='' | is.null(title_x),'',paste0(' xlab(','"',title_x,'"',') + ')),
    #                ifelse(title_y=='' | is.null(title_y),'',paste0(' ylab(','"',title_y,'"',') ')),
    #                ifelse(fontSize==10 & legendPos == 'right' ,'',paste0('+ theme(axis.text = element_text(size = ', fontSize,'),
    #                                                                    axis.title.x = element_text(size = ', fontSize,'),
    #                                                                    axis.title.y = element_text(size = ', fontSize,'),
    #                                                                    plot.title = element_text(size = ',fontSize,'),
    #                                                                    legend.position = ','"',legendPos,'"',')')))
    # 
    
    
    code <-  HTML(paste0('<pre>ggplot(data = ',df_name, ' , aes(x=', x, ', y=', y, ifelse(colorby=="None",")) + <br> ",paste0(',' ,'color = ',colorby, ')) + <br>')),
                         paste0('geom_line(',ifelse(lineType == 'solid' & lineSize == 1, paste0(') + '),
                                                    ifelse(lineType != 'solid' & lineSize == 1, paste0('linetype = ', '"' ,lineType, '"' , ') +'),
                                                           ifelse(lineType == 'solid' & lineSize != 1, paste0('size = ', lineSize, ') +'),
                                                                  ifelse(lineType != 'solid' & lineSize != 1, paste0('linetype = ', '"' ,lineType, '"' ,', size = ', lineSize, ') + <br>')))))),
                         ifelse(dots == TRUE, paste0('geom_point() + '),''),
                         paste0(ifelse(Theme=="NULL" | is.null(Theme),'',paste0('&ensp;',Theme,'+ <br>'))),
                         ifelse(plotTitle=='' | is.null(plotTitle),'',paste0('&ensp; labs(title = ','"',plotTitle,'"',') + <br>')),
                         ifelse(title_x=='' | is.null(title_x),'',paste0(' xlab(','"',title_x,'"',') + <br>')),
                         ifelse(title_y=='' | is.null(title_y),'',paste0(' ylab(','"',title_y,'"',') + <br>')),
                         ifelse(fontSize==10 & legendPos == 'right' , paste0('theme(axis.text.x = ', ifelse(hideAxis, 'element_blank())  <br>', paste0('element_text(angle = ', axisAngle, ', hjust = 1)) <br>'))),
                                paste0(paste0(' theme(axis.text = element_text(size = ', fontSize,'), <br>'),
                                       paste0(' &emsp; &emsp; &emsp; axis.title.x = element_text(size = ', fontSize,'),<br>'),
                                       paste0(' &emsp; &emsp; &emsp; axis.title.y = element_text(size = ', fontSize,'),<br>'),
                                       paste0(' &emsp; &emsp; &emsp; plot.title = element_text(size = ',fontSize,'),<br>'),
                                       paste0(' &emsp; &emsp; &emsp; legend.position = ','"',legendPos,'"',', <br>'),
                                       paste0(' &emsp; &emsp; &emsp; axis.text.x = ', ifelse(hideAxis, 'element_blank())', paste0('element_text(angle = ', axisAngle, ', hjust = 1))')))
                                       
                                )
                                
                         ),
                         ifelse(facetRow != 'None' & facetCol != 'None',paste0(" + <br> facet_grid(",as.formula(paste0(facetRow, "~", facetCol)),") " ),''),
                         ifelse(facetRow != 'None' & facetCol == 'None',paste0('+ <br> facet_grid(',facetRow,' ~ .) '),'' ),
                         ifelse(facetRow == 'None' & facetCol != 'None',paste0('+ <br> facet_grid(. ~ ',facetCol,') ') ,''),
                         '</pre>'))
    
    # facet
    if(facetRow != 'None' & facetCol != 'None'){
      p <-  p + facet_grid(as.formula(paste0(facetRow, "~", facetCol)))
      #code <- paste0(code,'+ facet_grid(',facetRow,' ~ ',facetCol,')')
    }
    if(facetRow != 'None' & facetCol == 'None'){
      p <-  p + facet_grid(as.formula(paste0(facetRow, "~ .")))
      #code <- paste0(code,'+ facet_grid(',facetRow,' ~ .)')
    }
    if(facetRow == 'None' & facetCol != 'None'){
      p <-  p + facet_grid(as.formula(paste0(". ~", facetCol)))
      #code <- paste0(code,'+ facet_grid(. ~ ',facetCol,')')
    }
    
    # if(hideAxis == TRUE){
    #   p <-  p + theme(axis.text.x = element_blank())
    #   code <- paste0(code, '+ theme(axis.text.x = element_blank())')
    # } else{
    #   if(axisAngle > 0){
    #     p <-  p + theme(axis.text.x = element_text(angle = axisAngle, hjust = 1))
    #     code <- paste0(code, '+ theme(axis.text.x = element_text(angle = ', axisAngle, ', hjust = 1))')
    #   }
    # }
    
    
    ls <- list()
    ls[['plot']] <- p
    ls[['code']] <- code
    return(ls)
    
    
    
    
    
  }
  