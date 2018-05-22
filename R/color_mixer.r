#' Create a vector of colors for the vertices
#' @param x A graph.
#' @param type Character. Either `edges` or `vertex`
#' @param ... Further arguments passed to the corresponding method.
#' @export
set_colors <- function(x, type, ...) {

  switch (type,
          edges  = set_edges_colors(x, ...),
          vertex = set_vertex_colors(x, ...)
  )
}

#' @rdname set_colors
#' @param vertex.color Vector of vertices colors.
#' @export
set_edges_colors <- function(x, vertex.color, ...) {

  # If no vertex color by default
  if (missing(vertex.color))
    vertex.color <- set_vertex_colors(x)

  # Creating the mix

}

#' @rdname set_colors
#' @param vattr Name of the vertex attribute.
#' @param is_categorical Logical. When `TRUE` sets the colors as categories.
#' @param color_map A function to generate a palette.
#' @details
#'
#' If no attribute is provided, then by defaul the colors are set accoring to
#' indegree.
#'
#' `x` can be either a graph of class `igraph` or `network`.
#'
#' @export
#' @examples
#' data(UKfaculty, package="igraphdata")
#' set_vertex_colors(UKfaculty, Group)
set_vertex_colors <- function(
  x,
  vattr,
  is_categorical = FALSE,
  color_map      = viridis::viridis
) {

  # Extracting attribute name
  if (!missing(vattr)) {
    vattr <- substitute(vattr)
    vattr <- get_vertex_attr(x, vattr)
  } else
    vattr <- calc_degree(x, mode. = "in")

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
