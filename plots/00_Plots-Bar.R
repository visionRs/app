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
                     facetCol) {
  
  p <- ggplot(data, aes_string(x, y, fill =ifelse(colorby == 'None', shQuote("None"), colorby) )) +
    geom_bar(stat="identity") +
    eval(parse(text=as.character(Theme))) +
    labs(title = plotTitle) +
    xlab(title_x) + ylab(title_y) +
    theme(axis.text = element_text(size = fontSize),
          axis.title.x = element_text(size = fontSize),
          axis.title.y = element_text(size = fontSize),
          plot.title = element_text(size = fontSize),
          legend.position = legendPos)

  if(colorby=='None'){
    
    p <- p + scale_fill_manual(values = colourfill) + theme(legend.position = 'none')
  }
  

  code <-paste0('ggplot(data,', 'aes(', x, ',', y, ifelse(colorby=="None",paste0(', fill=', colourfill, ')) + '),paste0(',' ,'fill = ',colorby, ')) +')),' 
                geom_bar(stat="identity") +',
                ifelse(Theme=="NULL" | is.null(Theme),'',paste0(Theme,'+ ')),
                ifelse(plotTitle=='' | is.null(plotTitle),'',paste0('labs(title = ','"',plotTitle,'"',') + ')),
                ifelse(title_x=='' | is.null(title_x),'',paste0(' xlab(','"',title_x,'"',') + ')),
                ifelse(title_y=='' | is.null(title_y),'',paste0(' ylab(','"',title_y,'"',') ')),
                ifelse(fontSize==10 & legendPos == 'right' ,'',
                paste0('+ theme(axis.text = element_text(size = ', fontSize,'),
                                axis.title.x = element_text(size = ', fontSize,'),
                                axis.title.y = element_text(size = ', fontSize,'),
                                plot.title = element_text(size = ',fontSize,'),
                                legend.position = ','"',legendPos,'"',')'))
                )
  
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
  
  
  
 
  ls <- list()
  ls[['plot']] <- p
  ls[['code']] <- code
  return(ls)
}# else ends for Theme



