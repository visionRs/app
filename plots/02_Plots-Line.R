#1 PLOTS CODE: --------------------------------

#1.1 PLOTS CODE: Line Plot---------------------
line_plot <- function(data=dt,x=NULL,y=NULL, colorby) {
  if(colorby == 'None'){
      p <-  ggplot(data, aes_string(x,y)) +
        geom_line()
      code <- paste0('ggplot(data, aes_string(',x,',',y,')) +
        geom_line()')
      ls <- list()
      ls[['plot']] <- p
      ls[['code']] <- code
      return(ls)
  } else{
      p <-  ggplot(data, aes_string(x,y, color = colorby)) +
        geom_line()
      code <- paste0('ggplot(data, aes_string(',x,',',y, ',' ,'color = ', colorby, ')) + 
                     geom_line()')
      ls <- list()
      ls[['plot']] <- p
      ls[['code']] <- code
      return(ls)
  }
}