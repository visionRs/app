#1 PLOTS CODE: --------------------------------

#1.1 PLOTS CODE: Bar Plot---------------------

bar_plot <- function(data=NULL,x=NULL,y=NULL) {
  p <- ggplot(data, aes_string(x,y)) +
  geom_bar(stat="identity")
  return(p)
}

#1.2 PLOTS CODE: Scatter Plot---------------------
scatter_plot <- function(data=dt,x=NULL,y=NULL) {
  p <- ggplot(data, aes_string(x,y)) +
    geom_point()
  return(p)
}


#1.3 PLOTS CODE: Line Plot---------------------
line_plot <- function(data=dt,x=NULL,y=NULL) {
  p <-  ggplot(data, aes_string(x,y)) +
    geom_line()
  return(p)
}