library(netplot)
library(igraph)
set.seed(21)
n <- 100
data(UKfaculty, package = "igraphdata")
x <- UKfaculty # sample_smallworld(1, n, 4, .2)
n <- vcount(x)
nam <- sapply(1:n, function(i) {
  nlen <- sample(5:20, 1)
  paste0(sample(letters, nlen, TRUE), collapse="")
  })
ans <- nplot2(
  x,
  vertex.label      = nam,
  vertex.size       = degree(x, mode = "in"),
  vertex.size.range = c(.01, .04, 4),
  vertex.color      = viridis::inferno(max(degree(x))+1)[degree(x)+1],
  vertex.label.col  =  "white",
  vertex.label.fontface = "bold"
  )

library(grid)
grid.newpage()
pushViewport(viewport())
grid.rect(gp=gpar(fill="black"))
popViewport()
pushViewport(viewport(width = .8, height = .8))
grid.draw(ans$grob)
# popViewport(0L)
