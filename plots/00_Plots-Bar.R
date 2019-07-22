#1 PLOTS CODE: --------------------------------

#1.1 PLOTS CODE: Bar Plot---------------------

bar_plot <- function(data=NULL,x=NULL,y=NULL, colorby) {
  if(colorby == 'None'){
    p <- ggplot(data, aes_string(x, y)) +
      geom_bar(stat="identity")
    code <- paste0('ggplot(data, aes_string(',x,',',y,')) +
              geom_bar(stat="identity")')
    ls <- list()
    ls[['plot']] <- p
    ls[['code']] <- code
    return(ls)
  } else{
    p <- ggplot(data, aes_string(x, y, fill = colorby)) +
      geom_bar(stat="identity")
    code <- paste0('ggplot(mtcars, aes_string(', x, ',', y, ',' ,'fill = ', colorby, ')) + 
                   geom_bar(stat="identity")')
    ls <- list()
    ls[['plot']] <- p
    ls[['code']] <- code
    return(ls)
  }
}
