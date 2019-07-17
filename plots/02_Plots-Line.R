#1 PLOTS CODE: --------------------------------

#1.1 PLOTS CODE: Line Plot---------------------
line_plot <- function(data=dt,x=NULL,y=NULL) {
  p <-  ggplot(data, aes_string(x,y)) +
    geom_line()
  code <- paste0('ggplot(data, aes_string(',x,',',y,')) +
    geom_line()')
  ls <- list()
  ls[['plot']] <- p
  ls[['code']] <- code
  return(ls)
}