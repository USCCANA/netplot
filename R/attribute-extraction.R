#' Extract a graph attribute
#' @param graph A graph object of class igraph or network.
#' @param attribute A character string specifying the name of the attribute.
#' @return A vector of the attribute values. If the attribute does not exist, an error is thrown.
#' @keywords internal
#' @noRd
get_vertex_attribute <- function(graph, attribute) UseMethod("get_vertex_attribute")

#' @keywords internal
get_vertex_attribute.igraph <- function(graph, attribute) {

  # Check if the attribute exists
  if (!(attribute %in% igraph::vertex_attr_names(graph))) {
    stop("Attribute does not exist in graph")
  }

  # Extract the attribute
  igraph::vertex_attr(graph, name = attribute)

}

#' @keywords internal
get_vertex_attribute.network <- function(graph, attribute) {

  # Check if the attribute exists
  if (!(attribute %in% network::list.vertex.attributes(graph))) {
    stop("Attribute does not exist in graph")
  }

  # Extract the attribute
  network::get.vertex.attribute(graph, attribute)

}

#' @keywords internal
get_vertex_attribute.default <- function(graph, attribute) {

  stop("Graph type not supported")

}

#' @keywords internal
get_edge_attribute <- function(graph, attribute) UseMethod("get_edge_attribute")

#' @keywords internal
get_edge_attribute.igraph <- function(graph, attribute) {

  # Check if the attribute exists
  if (!(attribute %in% igraph::edge_attr_names(graph))) {
    stop("Attribute does not exist in graph")
  }

  # Extract the attribute
  igraph::edge_attr(graph, name = attribute)

}

#' @keywords internal
get_edge_attribute.network <- function(graph, attribute) {

  # Check if the attribute exists
  if (!(attribute %in% network::list.edge.attributes(graph))) {
    stop("Attribute does not exist in graph")
  }

  # Extract the attribute
  network::get.edge.attribute(graph, attribute)

}

#' Retrieve all vertex/edge attributes of a graph as a named list
#'
#' Used as the data environment when evaluating attribute formulas (e.g.
#' `~ log(weight)`).
#' @param graph A graph object of class `igraph` or `network`.
#' @param type Either `"vertex"` or `"edge"`.
#' @return A named list, one element per attribute.
#' @noRd
all_graph_attributes <- function(graph, type = c("vertex", "edge")) {

  type <- match.arg(type)

  if (inherits(graph, "igraph")) {

    if (type == "vertex")
      igraph::vertex_attr(graph)
    else
      igraph::edge_attr(graph)

  } else if (inherits(graph, "network")) {

    nms <- if (type == "vertex")
      network::list.vertex.attributes(graph)
    else
      network::list.edge.attributes(graph)

    getter <- if (type == "vertex")
      network::get.vertex.attribute
    else
      network::get.edge.attribute

    stats::setNames(lapply(nms, function(a) getter(graph, a)), nms)

  } else {

    stop(
      "Attribute formulas are only supported for 'igraph' and 'network' ",
      "objects (got '", class(graph)[1], "').",
      call. = FALSE
    )

  }

}

#' Evaluate a one-sided formula against a graph's vertex/edge attributes
#'
#' The right-hand side of the formula is evaluated with the graph's attributes
#' available as variables, so both bare attribute names (`~ weight`) and
#' expressions (`~ log(weight)`, `~ 2 * weight`) are supported.
#' @param graph A graph object of class `igraph` or `network`.
#' @param formula A one-sided formula.
#' @param type Either `"vertex"` or `"edge"`.
#' @return The evaluated vector.
#' @noRd
eval_attribute_formula <- function(graph, formula, type = c("vertex", "edge")) {

  type   <- match.arg(type)
  attrs  <- all_graph_attributes(graph, type)
  rhs    <- formula[[length(formula)]]
  enclos <- environment(formula)
  if (is.null(enclos))
    enclos <- parent.frame()

  tryCatch(
    eval(rhs, envir = attrs, enclos = enclos),
    error = function(e) stop(
      "Could not evaluate the ", type, " formula `", deparse(rhs), "`. ",
      "Make sure the ", type, " attribute(s) it references exist in the graph. ",
      "Available ", type, " attributes: ",
      if (length(attrs)) paste(names(attrs), collapse = ", ") else "(none)",
      ". Original error: ", conditionMessage(e),
      call. = FALSE
    )
  )

}

#' @keywords internal
get_edge_attribute.default <- function(graph, attribute) {

  stop("Graph type not supported")

}