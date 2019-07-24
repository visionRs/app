#1 PLOTS CODE: --------------------------------


#1.1 PLOTS CODE: Scatter Plot---------------------
scatter_plot <- function(data=dt,x=NULL,y=NULL, colorby, fontSize, legendPos, dotSize) {
  if(colorby == 'None'){
      p <- ggplot(data, aes_string(x,y)) +
        geom_point(size = dotSize) +
        theme(axis.text = element_text(size = fontSize),
              axis.title.x = element_text(size = fontSize),
              axis.title.y = element_text(size = fontSize))
      
      code <- paste0('ggplot(',data,', aes(',x,',',y,')) +
                      geom_point(size = ',dotSize,') +
                      theme(axis.text = element_text(size = ', fontSize,'),
                               axis.title.x = element_text(size = ', fontSize,'),
                               axis.title.y = element_text(size = ', fontSize,'))')
      ls <- list()
      ls[['plot']] <- p
      ls[['code']] <- code
      return(ls)
  } else{
      p <- ggplot(data, aes_string(x,y, color = colorby)) +
        geom_point(size = dotSize) +
        theme(axis.text = element_text(size = fontSize),
              axis.title.x = element_text(size = fontSize),
              axis.title.y = element_text(size = fontSize),
              legend.position = legendPos)
      
      code <- paste0('ggplot(',data,', aes(',x,',',y, ',' ,'color = ', colorby, ')) + 
                      geom_point(size = ',dotSize,') +
                      theme(axis.text = element_text(size = ', fontSize,'),
                               axis.title.x = element_text(size = ', fontSize,'),
                               axis.title.y = element_text(size = ', fontSize,'),
                                legend.position = ','"',legendPos,'"',')')
      ls <- list()
      ls[['plot']] <- p
      ls[['code']] <- code
      return(ls)
  }
}