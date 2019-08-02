# Sourcing all R files from plot
Jmisc::sourceAll(here::here("plots"))


# List dfs
data(iris)
data("diamonds")
data("trees")
data("uspop")
data("rivers")
data("swiss")
#temp <- ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']
 temp <- Filter(function(x) is.data.frame(get(x)), ls()) 
