#' @title Bar Plot Code
#' @description Helper function to generate bar plot based on different conditions.
#' @param data data.frame - Passed from shiny enviornment
#' @param x character- This is x variable for aesthitic, supplied from shiny enviornment.
#' @param y character- This is y variable for aesthitic, supplied from shiny enviornment.
#' @param colorby character - Parameter used to colorby plot. (defaults to None).
#' @param Theme character- parameter to change plot theme. (defaults to None).
#' @param fontsize numeric- This parameter controls fontsize for x-y titles, x-y ticks labels. (defaults to 10).
#' @param legendPos character- Adjust Legend position (defaults to right)
#' @param title_x character- Set x axis title.
#' @param title_y character- Set y axis title.
#' @param plotTitle character- Set plot title.
#' @return returns H2OFrame
#' @export



#1 PLOTS CODE: --------------------------------

#1.1 PLOTS CODE: Bar Plot---------------------

bar_plot <- function(data=NULL,
                     df_name=NULL,
                     x=NULL,
                     y=NULL, 
                     colorby="None",
                     Theme=NULL, 
                     fontSize=10,
                     legendPos='right',
                     title_x='',
                     title_y='',
                     colourfill='#00FF0080',
                     plotTitle='',
                     facetRow,
                     facetCol,
                     hideAxis,
                     axisAngle=90,
                     position='',
                     coorflip=FALSE,
                     interactive=FALSE) {
  if(interactive==FALSE){
    
    
    
  p <- ggplot(data, aes_string(x, y, fill =ifelse(colorby == 'None', shQuote("None"), colorby) )) +
    geom_bar(stat="identity" , position = position) + #if(position=='') {} else { paste0(',', position = position)} ) +
    eval(parse(text=as.character(Theme))) +
    labs(title = plotTitle) +
    xlab(title_x) + ylab(title_y) +
    {if(coorflip) { coord_flip() } } +
    {if(colorby=='None') { scale_fill_manual(values = colourfill) } } +
    {if(facetRow != 'None' & facetCol != 'None'){facet_grid(as.formula(paste0(facetRow, "~", facetCol)))} 
      else if(facetRow != 'None' & facetCol == 'None'){facet_grid(as.formula(paste0(facetRow, "~ .")))}
      else if(facetRow == 'None' & facetCol != 'None'){facet_grid(as.formula(paste0(". ~", facetCol)))}
    } +
    theme(axis.text = element_text(size = fontSize),
          axis.title.x = element_text(size = fontSize),
          axis.title.y = element_text(size = fontSize),
          plot.title = element_text(size = fontSize),
          legend.position = if (colorby=='None') 'none' else legendPos,
          axis.text.x = if(hideAxis==TRUE) element_blank() else element_text(angle =axisAngle,hjust = 1)) 
          

  # if(colorby=='None'){
  #   
  #   p <- p + scale_fill_manual(values = colourfill)
  # }
  # 

  code <-HTML(paste0('<pre>ggplot(data = ',df_name, ' , aes(x=', x, ', y=', y, ifelse(colorby=="None",")) + <br> ",paste0(',' ,'fill = ',colorby, ')) + <br>')), 
                     paste0('geom_bar(stat="identity" , position = ',shQuote(position)), ifelse(colorby=="None",paste0(', fill =',shQuote(colourfill), ') + <br>'), ') + <br>'),
                     paste0(ifelse(Theme=="NULL" | is.null(Theme),'',paste0('&ensp;',Theme,'+ <br>'))),
                     ifelse(plotTitle=='' | is.null(plotTitle),'',paste0('&ensp; labs(title = ','"',plotTitle,'"',') + <br>')),
                     ifelse(title_x=='' | is.null(title_x),'',paste0(' xlab(','"',title_x,'"',') + <br>')),
                     ifelse(title_y=='' | is.null(title_y),'',paste0(' ylab(','"',title_y,'"',') ')),
                     ifelse(fontSize==10 & legendPos == 'right' , paste0(' + <br> theme(axis.text.x = ', ifelse(hideAxis, 'element_blank())  <br>', paste0('element_text(angle = ', axisAngle, ', hjust = 1)) <br>'))),
                            paste0(paste0('+ <br> theme(axis.text = element_text(size = ', fontSize,'), <br>'),
                                   paste0(' &emsp; &emsp; &emsp; axis.title.x = element_text(size = ', fontSize,'),<br>'),
                                   paste0(' &emsp; &emsp; &emsp; axis.title.y = element_text(size = ', fontSize,'),<br>'),
                                   paste0(' &emsp; &emsp; &emsp; plot.title = element_text(size = ',fontSize,'),<br>'),
                                   paste0(' &emsp; &emsp; &emsp; legend.position = ','"',legendPos,'"',')<br>'),
                                   paste0(' &emsp; &emsp; &emsp; axis.text.x = ', ifelse(hideAxis, 'element_blank())', paste0('element_text(angle = ', axisAngle, ', hjust = 1))')))
                                   
                                   )
                              
                            ),
                     ifelse(facetRow != 'None' & facetCol != 'None',paste0(" + <br> facet_grid(",as.formula(paste0(facetRow, "~", facetCol)),") " ),''),
                     ifelse(facetRow != 'None' & facetCol == 'None',paste0('+ <br> facet_grid(',facetRow,' ~ .) '),'' ),
                     ifelse(facetRow == 'None' & facetCol != 'None',paste0('+ <br> facet_grid(. ~ ',facetCol,') ') ,''),
                     ifelse(coorflip==TRUE, paste0('+ <br> coord_flip()'),paste0('')),
                     '</pre>'))
  
  # # facet
  # if(facetRow != 'None' & facetCol != 'None'){
  #   p <-  p + facet_grid(as.formula(paste0(facetRow, "~", facetCol)))
  #   #code <- paste0(code,'+ facet_grid(',facetRow,' ~ ',facetCol,')')
  # }
  # if(facetRow != 'None' & facetCol == 'None'){
  #   p <-  p + facet_grid(as.formula(paste0(facetRow, "~ .")))
  #  # code <- paste0(code,'+ facet_grid(',facetRow,' ~ .)')
  # }
  # if(facetRow == 'None' & facetCol != 'None'){
  #   p <-  p + facet_grid(as.formula(paste0(". ~", facetCol)))
  #   #code <- paste0(code,'+ facet_grid(. ~ ',facetCol,')')
  # }
  # 
  
  #p <- p + ifelse(hideAxis==TRUE, theme(axis.text.x =element_blank()), theme(axis.text.x=element_text(angle = axisAngle, hjust = 1)))
  #code <- paste0(code, " + " ,ifelse(hideAxis==TRUE, 'theme(axis.text.x =element_blank())', 'theme(axis.text.x=element_text(angle = axisAngle, hjust = 1))'))
  # if(hideAxis == TRUE ){
  #   p <-  p + theme(axis.text.x = element_blank())
  #   code <- paste0(code, '+ theme(axis.text.x = element_blank())',hideAxis)
  # } else if (axisAngle > 0 & hideAxis==FALSE) {
  #   
  #   p <-  p + theme(axis.text.x = element_text(angle = axisAngle, hjust = 1))
  #   code <- paste0(code, '+ theme(axis.text.x = element_text(angle = ', axisAngle, ', hjust = 1))')
  # }
  
  # if(axisAngle > 0){
  #   p <-  p + theme(axis.text.x = element_text(angle = axisAngle, hjust = 1))
  #   code <- paste0(code, '+ theme(axis.text.x = element_text(angle = ', axisAngle, ', hjust = 1))')
  # }
  # 
  
  if(hideAxis == TRUE){
    p <-  p + theme(axis.text.x = element_blank())
    #code <- paste0(code, '+ theme(axis.text.x = element_blank())')
  } else{
    if(axisAngle > 0){
      p <-  p + theme(axis.text.x = element_text(angle = axisAngle, hjust = 1))
     # code <- paste0(code, '+ theme(axis.text.x = element_text(angle = ', axisAngle, ', hjust = 1))')
    }
  }
 
  ls <- list()
  ls[['plot']] <- p
  ls[['code']] <- code
  return(ls)
  
  } else {
  
    
  #data %>% 
  
  
} # else ends for Theme


} #function ends here
