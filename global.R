
# List dfs
data(iris)
temp <- ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']
  

