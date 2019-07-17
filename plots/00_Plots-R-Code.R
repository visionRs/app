#1 PLOTS CODE: --------------------------------

#1.1 PLOTS CODE: Bar Plot---------------------

bar_plot <- function(data=NULL,x=NULL,y=NULL) {
  p <- ggplot(data, aes_string(x,y)) +
  geom_bar(stat="identity")
  code <- paste0('ggplot(data, aes_string(',x,',',y,')) +
            geom_bar(stat="identity")')
  ls <- list()
  ls[['plot']] <- p
  ls[['code']] <- code
  return(ls)
}

#1.2 PLOTS CODE: Scatter Plot---------------------
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


#1.3 PLOTS CODE: Line Plot---------------------
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