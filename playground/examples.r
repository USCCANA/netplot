library(netdiffuseR)

x <- igraph::graph_from_edgelist(
  matrix(
    c(1,2,2,1,3,4,4,3,4,4), ncol=2, byrow = TRUE
  )
)

layout <- matrix(c(
  1,1,2,2,3,3,4,4
), byrow = TRUE, ncol=2)

vertex.size <- c(.25, .25, .5, .5)
nplot(x, layout = layout, vertex.size = vertex.size, vertex.size.range = c(.1,.3))

nplot(netdiffuseR::diffnet_to_igraph(medInnovationsDiffNet)[[1]])

nplot(igraph::rewire(igraph::barabasi.game(200, m=2, power = .95), igraph::each_edge(.1)))
