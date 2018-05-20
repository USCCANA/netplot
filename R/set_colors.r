#' Create a vector of colors for the vertices
#' @param x A graph.
#' @param type Character. Either `edges` or `vertex`
#' @export
set_colors <- function(x, type, ...) {

  switch (type,
    edges  = set_edges_colors(x, ...),
    vertex = set_vertex_colors(x, ...)
  )
}

set_edges_colors <- function(x, ...) {
  NULL
}

#' @rdname set_colors
#' @param vattr Character. Name of the vertex attribute.
#' @param is_categorical Logical. When `TRUE` sets the colors as categories.
#' @param color_map A function to generate a palette.
#' @export
set_vertex_colors <- function(
  x,
  vattr,
  is_categorical = FALSE,
  color_map      = viridis::viridis
  ) {

  # Extracting attribute name
  if (!missing(vattr)) {
    vattr <- get_vertex_attr(x, vattr)
  } else
    vattr <- calc_degree(x, mode = "in")

  # What type of attribute?
  if (!is_categorical) {

    # Is is numeric?
    if (!is.numeric(vattr))
      stop("The selected variable is not numeric.", call. = FALSE)

    # Moving to 0-1 scale
    vattr     <- (vattr - min(vattr))/(diff(range(vattr, na.rm = TRUE)) + 1e-15)
    color_map <- grDevices::colorRamp(color_map(20))
    color     <- grDevices::rgb(color_map(vattr), maxColorValue = 255)

  } else {

    # Turning the data into factor
    vattr <- as.factor(vattr)

    # Generating as many colors as needed
    color_map <- structure(color_map(nlevels(vattr)), names= levels(vattr))
    color     <- color_map[as.integer(vattr)]

  }

  structure(
    .Data = color,
    color_map = color_map
  )

}

# Misc

# Wrappers for different graph methods -----------------------------------------

stop_unsopported_graph <- function(x) {
  stop("Graph of class `", class(x), "` is not supported.", call. = FALSE)
}

get_vertex_attr <- function(x, attr.) {

  if (inherits(x, "igraph"))
    igraph::vertex_attr(x, attr.)
  else if (inherits(x, "network"))
    network::get.vertex.attribute(x, attr.)
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




