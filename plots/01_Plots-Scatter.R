#1 PLOTS CODE: --------------------------------


#1.1 PLOTS CODE: Scatter Plot---------------------
scatter_plot <- function(data=dt,x=NULL,y=NULL, colorby, fontSize) {
  if(colorby == 'None'){
      p <- ggplot(data, aes_string(x,y)) +
        geom_point() +
        theme(axis.text = element_text(size = fontSize),
              axis.title.x = element_text(size = fontSize),
              axis.title.y = element_text(size = fontSize))
      
      code <- paste0('ggplot(data, aes_string(',x,',',y,')) +
                      geom_point() +
                      theme(axis.text = element_text(size = ', fontSize,'),
                               axis.title.x = element_text(size = ', fontSize,'),
                               axis.title.y = element_text(size = ', fontSize,'))')
      ls <- list()
      ls[['plot']] <- p
      ls[['code']] <- code
      return(ls)
  } else{
      p <- ggplot(data, aes_string(x,y, color = colorby)) +
        geom_point() +
        theme(axis.text = element_text(size = fontSize),
              axis.title.x = element_text(size = fontSize),
              axis.title.y = element_text(size = fontSize))
      
      code <- paste0('ggplot(data, aes_string(',x,',',y, ',' ,'color = ', colorby, ')) + 
                      geom_point() +
                      theme(axis.text = element_text(size = ', fontSize,'),
                               axis.title.x = element_text(size = ', fontSize,'),
                               axis.title.y = element_text(size = ', fontSize,'))')
      ls <- list()
      ls[['plot']] <- p
      ls[['code']] <- code
      return(ls)
  }
}