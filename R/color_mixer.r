#' Create a vector of colors for vertices and edges
#'
#' Using `vertex`/`edge` attributes, these functions return vectors of colors
#' that can be used either during the creation of the [nplot] object, or
#' afterwards when changing `gpar` (graphical parameter) values with [set_gpar].
#'
#' @param x A graph.
#' @param dat A vector of data to generate the color from.
#' @param vattr,eattr Character. Names of either vertex or edge variables to be
#' used for generating the colors.
#' @param categorical Logical. When `TRUE` sets the colors as categories.
#' @param color_map A function to generate a palette.
#' @param ... Further arguments passed to `make_colors`.
#' @examples
#'
#' data(UKfaculty, package="igraphdata")
#' col <- make_vertex_colors(UKfaculty, "Group")
#'
#' if (require("magrittr")) {
#'   library(magrittr)
#'
#'   nplot(UKfaculty) %>%
#'     set_vertex_gpar("core", fill = col, col=col) %>%
#'     set_vertex_gpar("frame", fill = col, col=col, alpha=.7) %>%
#'     set_edge_gpar(col="gray50", fill="gray50", alpha=.5)
#' }
#' @export
make_colors <- function(
  dat,
  categorical = FALSE,
  color_map   = grDevices::hcl.colors
  ) {

  # What type of attribute?
  if (!categorical) {

    # Is is numeric?
    if (!is.numeric(dat))
      stop("The selected variable is not numeric.", call. = FALSE)

    # Moving to 0-1 scale
    dat     <- (dat - min(dat))/(diff(range(dat, na.rm = TRUE)) + 1e-15)
    color_map <- grDevices::colorRamp(color_map(20))
    color     <- grDevices::rgb(color_map(dat), maxColorValue = 255)

  } else {

    # Turning the data into factor
    dat <- as.factor(dat)

    # Generating as many colors as needed
    color_map <- structure(color_map(nlevels(dat)), names= levels(dat))
    color     <- color_map[as.integer(dat)]

  }

  structure(
    .Data = color,
    color_map = color_map
  )
}

#' @rdname make_colors
#' @export
make_edges_colors <- function(
  x,
  eattr,
  ...
  ) {


  make_colors(dat = get_edge_attr(x, eattr), ...)

}

#' @rdname make_colors
#' @details
#'
#' If no attribute is provided, then by defaul the colors are set accoring to
#' indegree.
#'
#' `x` can be either a graph of class `igraph` or `network`.
#'
#' @export
make_vertex_colors <- function(
  x,
  vattr,
  ...
) {

  if (!missing(vattr)) {
    vattr <- get_vertex_attr(x, vattr)
  } else
    vattr <- calc_degree(x, mode. = "in")

  make_colors(dat = vattr, ...)

}
