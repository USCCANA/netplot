library(netplot)
library(igraph)
set.seed(21)
n <- 100
data(UKfaculty, package = "igraphdata")

n <- vcount(UKfaculty)
nam <- sample(babynames::babynames$name, n)

ans <- nplot(
  UKfaculty,
  vertex.label      = nam,
  vertex.size.range = c(.01, .04, 4),
  vertex.label.col  =  "white",
  vertex.label.fontface = "bold",
  bg.col            = "black"
  )

ans$childrenOrder <- with(
  ans,
  c(
    childrenOrder[grepl("^edge[0-9]+[-][0-9]+$", childrenOrder)],
    childrenOrder[grepl("^vertex[.][0-9]+$", childrenOrder)],
    childrenOrder[grepl("^vertex-label[.][0-9]+$", childrenOrder)]
  ))


ans
