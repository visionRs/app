#1 PLOTS CODE: --------------------------------

#1.1 PLOTS CODE: Bar Plot---------------------

box_plot <- function(data=NULL,x=NULL,y=NULL) {
  
  p <- ggplot(iris, aes_string(paste0("factor(",x,")"),y)) +
    geom_boxplot()
  
  code <- paste0('ggplot(iris, aes_string(paste0("factor(",',x,',")"),',y,')) +
    geom_boxplot()')
  
  ls <- list()
  ls[['plot']] <- p
  ls[['code']] <- code
  return(ls)
}