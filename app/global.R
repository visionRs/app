# Sourcing all R files from plot
Jmisc::sourceAll(here::here("plots"))


# List dfs
data(iris)
temp <- ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']
  
