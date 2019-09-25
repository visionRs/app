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
                         df_name=NULL,
                         x=NULL,
                         y=NULL, 
                         Theme=NULL, 
                         colourfill='#2219CCCC', 
                         colorby='None', 
                         shapeby,
                         fontSize=10, 
                         legendPos='right', 
                         dotSize=2, 
                         dotOpa=0.7, 
                         title_x='', 
                         title_y ='', 
                         plotTitle ='',
                         hideAxis,
                         axisAngle,
                         regressionLine=FALSE, 
                         correlation=FALSE,
                         facetRow,
                         facetCol) {
  
  
  p <- ggplot(data,
              aes_string(x, y, color = ifelse(colorby == 'None','NULL',colorby), shape = ifelse(shapeby == 'None','NULL',shapeby))) +
              {if(colorby == "None") geom_point(size = dotSize, alpha = dotOpa, color=colourfill) else geom_point(size = dotSize, alpha = dotOpa)} +
              eval(parse(text=as.character(Theme))) +
              labs(title = plotTitle) +
              xlab(title_x) + ylab(title_y) +
              theme(axis.text = element_text(size = fontSize),
                    axis.title.x = element_text(size = fontSize),
                    axis.title.y = element_text(size = fontSize),
                    plot.title = element_text(size = fontSize),
                    legend.position = legendPos,
                    axis.text.x = if(hideAxis==TRUE) element_blank() else element_text(angle =axisAngle,hjust = 1))
  # 
  # 
  # # if(colorby == "None"){
  # #   p <- p + geom_point(size = dotSize, alpha = dotOpa, color=colourfill)
  # #     
  # # } else{
  # #   p <- p + geom_point(size = dotSize, alpha = dotOpa)
  # #    
  # # }
  # 
  # # p <-  p + eval(parse(text=as.character(Theme))) +
  # #   labs(title = plotTitle) +
  # #   xlab(title_x) + ylab(title_y) +
  # #   theme(axis.text = element_text(size = fontSize),
  # #         axis.title.x = element_text(size = fontSize),
  # #         axis.title.y = element_text(size = fontSize),
  # #         plot.title = element_text(size = fontSize),
  # #         legend.position = legendPos)
  # 
  # code <- paste0('ggplot(data,', 'aes(', x, ',', y, ifelse(colorby=='None' & shapeby == 'None',')) +',
  #                                                          ifelse(colorby == 'None' & shapeby != 'None', paste0(',' ,'shape = ',shapeby, ')) +'),
  #                                                                 ifelse(colorby != 'None' & shapeby == 'None', paste0(',' ,'color = ',colorby, ')) +'),
  #                                                                        ifelse(colorby !='None' & shapeby != 'None', paste0(',' ,'color = ',colorby, ',' ,'shape = ',shapeby, ')) +'))
  #                                                                 ))),
  #                 'geom_point(size = ',dotSize, ',alpha = ',dotOpa, ifelse(colorby=='NULL', paste0(', colour = ', '"' ,colourfill,'"',') +'),' ) +'),
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
  if(regressionLine == TRUE & correlation == TRUE){
    p <- p + geom_smooth(method=lm, se=FALSE) + stat_cor(method = "pearson")

    #code <-  paste0(code, '+ geom_smooth(method=lm, se=FALSE) + stat_cor(method = "pearson")')
  } else if(regressionLine == TRUE & correlation == "NULL") {
    p <- p + geom_smooth(method=lm, se=FALSE)
    #code <-  paste0(code, '+ geom_smooth(method=lm, se=FALSE)')
  }

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
  # 
  # if(hideAxis == TRUE){
  #   p <-  p + theme(axis.text.x = element_blank())
  #   code <- paste0(code, '+ theme(axis.text.x = element_blank())')
  # } else{
  #   if(axisAngle > 0){
  #     p <-  p + theme(axis.text.x = element_text(angle = axisAngle, hjust = 1))
  #     code <- paste0(code, '+ theme(axis.text.x = element_text(angle = ', axisAngle, ', hjust = 1))')
  #   }
  # }

  
  # p <- ggplot(data,
  #             aes_string(x, y, color = ifelse(colorby == 'None','NULL',colorby), shape = ifelse(shapeby == 'None','NULL',shapeby))) +
  #   if(colorby == "None") geom_point(size = dotSize, alpha = dotOpa, color=colourfill) else geom_point(size = dotSize, alpha = dotOpa) +
  #   eval(parse(text=as.character(Theme))) +
  #   labs(title = plotTitle) +
  #   xlab(title_x) + ylab(title_y) +
  #   {if(regressionLine == TRUE & correlation == TRUE) geom_smooth(method=lm, se=FALSE) + stat_cor(method = "pearson")
  #     else if(regressionLine == TRUE & correlation == "NULL") geom_smooth(method=lm, se=FALSE) } +
  #   {if(facetRow != 'None' & facetCol != 'None'){facet_grid(as.formula(paste0(facetRow, "~", facetCol)))}
  #     else if(facetRow != 'None' & facetCol == 'None'){facet_grid(as.formula(paste0(facetRow, "~ .")))}
  #     else if(facetRow == 'None' & facetCol != 'None'){facet_grid(as.formula(paste0(". ~", facetCol)))}
  #   } +
  #   theme(axis.text = element_text(size = fontSize),
  #         axis.title.x = element_text(size = fontSize),
  #         axis.title.y = element_text(size = fontSize),
  #         plot.title = element_text(size = fontSize),
  #         legend.position = legendPos,
  #         axis.text.x = if(hideAxis==TRUE) element_blank() else element_text(angle =axisAngle,hjust = 1))
  # 
  # 
  
  
  
  
  
  
  code <-  HTML(paste0('<pre>ggplot(data = ',df_name, ' , aes(x=', x, ', y=', y, ifelse(colorby=="None" & shapeby == 'None',")) + <br> ",ifelse(colorby == 'None' & shapeby != 'None', paste0(',' ,'shape = ',shapeby, ')) + <br>'),
                                                                                                                                                ifelse(colorby != 'None' & shapeby == 'None', paste0(',' ,'color = ',colorby, ')) + <br>'),
                                                                                                                                                       ifelse(colorby !='None' & shapeby != 'None', paste0(',' ,'color = ',colorby, ',' ,'shape = ',shapeby, ')) + <br>'))
                                                                                                                                                ))),
                       paste0('geom_point(size = ',dotSize,', alpha = ',dotOpa), ifelse(colorby=="None",paste0(', colour = ',shQuote(colourfill), ') + <br>'), ') + <br>'),
                       ifelse(regressionLine == TRUE  & correlation == TRUE, paste0("geom_smooth(method=lm, se=FALSE) + stat_cor(method = 'pearson') + <br>"),''),
                       ifelse(regressionLine == TRUE & correlation == "NULL", paste0("geom_smooth(method=lm, se=FALSE) + <br>"),''),
                       
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
  
  
  
  ls <- list()
  ls[['plot']] <- p
  ls[['code']] <- code
  return(ls)
  
}  
 