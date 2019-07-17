#1 PLOTS CODE: --------------------------------


#1.1 PLOTS CODE: Scatter Plot---------------------
scatter_plot <- function(data=dt,x=NULL,y=NULL) {
  p <- ggplot(data, aes_string(x,y)) +
    geom_point()
  code <- paste0('ggplot(data, aes_string(',x,',',y,')) +
    geom_point()')
  ls <- list()
  ls[['plot']] <- p
  ls[['code']] <- code
  return(ls)
}