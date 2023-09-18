#' Extract a graph attribute
#' @param graph A graph object of class igraph or network.
#' @param attribute A character string specifying the name of the attribute.
#' @return A vector of the attribute values. If the attribute does not exist, an error is thrown.
#' @noRd
get_vertex_attribute <- function(graph, attribute) UseMethod("get_vertex_attribute")

get_vertex_attribute.igraph <- function(graph, attribute) {

  # Check if the attribute exists
  if (!(attribute %in% igraph::vertex_attr_names(graph))) {
    stop("Attribute does not exist in graph")
  }

  # Extract the attribute
  igraph::vertex_attr(graph, name = attribute)

}

get_vertex_attribute.network <- function(graph, attribute) {

  # Check if the attribute exists
  if (!(attribute %in% network::list.vertex.attributes(graph))) {
    stop("Attribute does not exist in graph")
  }

  # Extract the attribute
  network::get.vertex.attribute(graph, attribute)

}

get_vertex_attribute.default <- function(graph, attribute) {

  stop("Graph type not supported")

}

get_edge_attribute <- function(graph, attribute) UseMethod("get_edge_attribute")

get_edge_attribute.igraph <- function(graph, attribute) {

  # Check if the attribute exists
  if (!(attribute %in% igraph::edge_attr_names(graph))) {
    stop("Attribute does not exist in graph")
  }

  # Extract the attribute
  igraph::edge_attr(graph, name = attribute)

}

get_edge_attribute.network <- function(graph, attribute) {

  # Check if the attribute exists
  if (!(attribute %in% network::list.edge.attributes(graph))) {
    stop("Attribute does not exist in graph")
  }

  # Extract the attribute
  network::get.edge.attribute(graph, attribute)

}

get_edge_attribute.default <- function(graph, attribute) {

  stop("Graph type not supported")

}