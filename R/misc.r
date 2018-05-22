# Misc

# Wrappers for different graph methods -----------------------------------------

stop_unsopported_graph <- function(x) {
  stop("Graph of class `", class(x), "` is not supported.", call. = FALSE)
}

get_vertex_attr <- function(x, attr.) {

  if (inherits(x, "igraph"))
    eval(attr., igraph::vertex.attributes(x))
  else if (inherits(x, "network"))
    network::get.vertex.attribute(x, as.character(attr.))
  else
    stop_unsopported_graph(x)

}

calc_degree <- function(x, mode. = "in") {
  if (inherits(x, "igraph"))
    igraph::degree(x, mode = mode.)
  else if (inherits(x, "network"))
    sna::degree(x, cmode = match.arg(
      mode., c("indegree", "outdegree", "freeman")
    ))
  else
    stop_unsopported_graph(x)

}




