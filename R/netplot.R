#' A wrapper of `colorRamp2`
#' @param i,j Integer scalar. Indices of ego and alter from 1 through n.
#' @param p Numeric scalar from 0 to 1. Proportion of mixing.
#' @param vcols Vector of colors.
#' @param alpha Numeric scalar from 0 to 1. Passed to [colorRamp2]
#' @return A color.
#' @noRd
edge_color_mixer <- function(i, j, vcols, p = .5, alpha = .15) {

  alphacolor(
    sprintf(
      "%sFF", grDevices::rgb(
        colorRamp2(vcols[c(i,j)], alpha = FALSE)(p),
        maxColorValue = 255
        )
      ), alpha.f = alpha
    )

}

#' Plot a network
#'
#' This is a description.
#'
#' @param x A graph. It supports networks stored as `igraph`, `network`, and
#' matrices objects (see details).
#' @param bg.col Color of the background.
#' @param layout Numeric two-column matrix with the graph layout in x/y positions of the vertices.
#' @param vertex.size Numeric vector of length `vcount(x)`. Absolute size of the vertex from 0 to 1.
#' @param vertex.nsides Numeric vector of length `vcount(x)`. Number of sizes of
#' the vertex. E.g. three is a triangle, and 100 approximates a circle.
#' @param vertex.color Vector of length `vcount(x)`. Vertex HEX or built in colors.
#' @param vertex.size.range Numeric vector of length 3. Relative size for the
#' minimum and maximum of the plot, and curvature of the scale. The third number
#' is used as `size^rel[3]`.
#' @param vertex.frame.color Vector of length `vcount(x)`. Border of vertex in
#' HEX or built in colors.
#' @param vertex.frame.prop Vector of length `vcount(x)`. What proportion of the
#' vertex does the frame occupy (values between 0 and 1).
#' @param vertex.rot Vector of length `vcount(x)` in Radians. Passed to [npolygon],
#' elevation degree from which the polygon is drawn.
#' @param vertex.label Character vector of length `vcount(x)`. Labels.
#' @param vertex.label.fontsize Numeric vector.
#' @param vertex.label.color Vector of colors of length `vcount(x)`.
#' @param vertex.label.fontfamily Character vector of length `vcount(x)`.
#' @param vertex.label.fontface See [grid::gpar]
#' @param vertex.label.show Numeric scalar. Proportion of labels to show as the
#' top ranking according to `vertex.size`.
#' @param vertex.label.range Numeric vector of size 2 or 3. Relative scale of
#' `vertex.label.fontsize` in points (see [grid::gpar]).
#' @param edge.color A vector of length `ecount(x)`. In HEX or built in colors.
#' Can be `NULL` in which case
#' the color is picked as a mixture between ego and alters' `vertex.color` values.
#' @param edge.width Vector of length `ecount(x)` from 0 to 1. All edges will be
#' the same size.
#' @param edge.width.range Vector of length `ecount(x)` from 0 to 1. Adjusting
#' width according to weight.
#' @param edge.arrow.size Vector of length `ecount(x)` from 0 to 1.
#' @param edge.curvature Numeric vector of length `ecount(x)`. Curvature of edges
#' in terms of radians.
#' @param edge.line.lty Vector of length `ecount(x)`. Line types in R
#' (e.g.- 1 = Solid, 2 = Dashed, etc).
#' @param edge.line.breaks Vector of length `ecount(x)`. Number of vertices to
#' draw (approximate) the arc (edge).
#' @param sample.edges Numeric scalar between 0 and 1. Proportion of edges to
#' sample.
#' @param skip.vertex,skip.edges,skip.arrows Logical scalar. When `TRUE` the
#' object is not plotted.
#' @param add Logical scalar.
#' @param zero.margins Logical scalar.
#' @importFrom igraph layout_with_fr degree vcount ecount
#' @importFrom grDevices adjustcolor rgb
#' @importFrom graphics lines par plot polygon rect segments plot.new plot.window
#' @import grid
#'
#' @details
#' When `x` is of class [matrix], it will be passed to [igraph::graph_from_adjacency_matrix()].
#'
#' In the case of `edge.color`, the user can specify colors using [netplot-formulae].
#' @return An object of class `c("netplot", "gTree", "grob", "gDesc")`. The object
#' has an additional set of attributes:
#' * `.xlim, .ylim` vector of size two with the x-asis/y-axis limits.
#' * `.layout` A numeric matrix of size `vcount(x) * 2` with the vertices positions
#' * `.edgelist` A numeric matrix, The edgelist.
#'
#' @examples
#' library(igraph)
#' library(netplot)
#' set.seed(1)
#' x <- sample_smallworld(1, 200, 5, 0.03)
#'
#' plot(x) # ala igraph
#' nplot(x) # ala netplot
#' @name nplot
#' @aliases netplot
NULL



#' @export
#' @rdname nplot
nplot <- function(
  x,
  layout,
  vertex.size             = 1,
  bg.col                  = "transparent",
  vertex.nsides           = 10,
  vertex.color            = grDevices::hcl.colors(1),
  vertex.size.range       = c(.01, .03, 4),
  vertex.frame.color      = NULL,
  vertex.rot              = 0,
  vertex.frame.prop       = .2,
  vertex.label            = NULL,
  vertex.label.fontsize   = NULL,
  vertex.label.color      = adjustcolor("black", alpha.f = .80),
  vertex.label.fontfamily = "sans",
  vertex.label.fontface   = "plain",
  vertex.label.show       = .3,
  vertex.label.range      = c(5, 15),
  edge.width              = 1,
  edge.width.range        = c(1, 2),
  edge.arrow.size         = NULL,
  edge.color              = ~ ego(alpha = .1, col = "gray") + alter,
  edge.curvature          = pi/3,
  edge.line.lty           = "solid",
  edge.line.breaks        = 5,
  sample.edges            = 1,
  skip.vertex             = FALSE,
  skip.edges              = FALSE,
  skip.arrows             = skip.edges,
  add                     = FALSE,
  zero.margins            = TRUE,
  edgelist
  ) {

  UseMethod("nplot")

}

#' @export
#' @rdname nplot
nplot.igraph <- function(
  x,
  layout                  = igraph::layout_nicely(x),
  vertex.size             = igraph::degree(x, mode="in"),
  bg.col                  = "transparent",
  vertex.nsides           = 10,
  vertex.color            = grDevices::hcl.colors(1),
  vertex.size.range       = c(.01, .03, 4),
  vertex.frame.color      = NULL,
  vertex.rot              = 0,
  vertex.frame.prop       = .2,
  vertex.label            = igraph::vertex_attr(x, "name"),
  vertex.label.fontsize   = NULL,
  vertex.label.color      = adjustcolor("black", alpha.f = .80),
  vertex.label.fontfamily = "sans",
  vertex.label.fontface   = "plain",
  vertex.label.show       = .3,
  vertex.label.range      = c(5, 15),
  edge.width              = igraph::edge_attr(x, "weight"),
  edge.width.range        = c(1, 2),
  edge.arrow.size         = NULL,
  edge.color              = ~ ego(alpha = .1, col = "gray") + alter,
  edge.curvature          = pi/3,
  edge.line.lty           = "solid",
  edge.line.breaks        = 5,
  sample.edges            = 1,
  skip.vertex             = FALSE,
  skip.edges              = FALSE,
  skip.arrows             = !igraph::is_directed(x),
  add                     = FALSE,
  zero.margins            = TRUE,
  edgelist
  ) {

  if (!length(edge.width))
    edge.width <- 1L

  nplot.default(
    x = x,
    layout = layout,
    vertex.size = vertex.size,
    bg.col = bg.col,
    vertex.nsides = vertex.nsides,
    vertex.color = vertex.color,
    vertex.size.range = vertex.size.range,
    vertex.frame.color = vertex.frame.color,
    vertex.rot = vertex.rot,
    vertex.frame.prop = vertex.frame.prop,
    vertex.label = vertex.label,
    vertex.label.fontsize = vertex.label.fontsize,
    vertex.label.color = vertex.label.color,
    vertex.label.fontfamily = vertex.label.fontfamily,
    vertex.label.fontface = vertex.label.fontface,
    vertex.label.show = vertex.label.show,
    vertex.label.range = vertex.label.range,
    edge.width = edge.width,
    edge.width.range = edge.width.range,
    edge.arrow.size = edge.arrow.size,
    edge.color = edge.color,
    edge.curvature = edge.curvature,
    edge.line.lty = edge.line.lty,
    edge.line.breaks = edge.line.breaks,
    sample.edges = sample.edges,
    skip.vertex = skip.vertex,
    skip.edges = skip.edges,
    skip.arrows = skip.arrows,
    add = add,
    zero.margins = zero.margins,
    edgelist = igraph::as_edgelist(x, names = FALSE)
  )

}

#' @export
#' @rdname nplot
#' @importFrom network as.edgelist
#' @importFrom sna gplot.layout.kamadakawai
nplot.network <- function(
  x,
  layout                  = sna::gplot.layout.kamadakawai(x, NULL),
  vertex.size             =  sna::degree(x, cmode="indegree"),
  bg.col                  = "transparent",
  vertex.nsides           = 10,
  vertex.color            = grDevices::hcl.colors(1),
  vertex.size.range       = c(.01, .03, 4),
  vertex.frame.color      = NULL,
  vertex.rot              = 0,
  vertex.frame.prop       = .2,
  vertex.label            =  network::get.vertex.attribute(x, "vertex.names"),
  vertex.label.fontsize   = NULL,
  vertex.label.color      = adjustcolor("black", alpha.f = .80),
  vertex.label.fontfamily = "sans",
  vertex.label.fontface   = "plain",
  vertex.label.show       = .3,
  vertex.label.range      = c(5, 15),
  edge.width              = 1,
  edge.width.range        = c(1, 2),
  edge.arrow.size         = NULL,
  edge.color              = ~ ego(alpha = .1, col = "gray") + alter,
  edge.curvature          = pi/3,
  edge.line.lty           = "solid",
  edge.line.breaks        = 5,
  sample.edges            = 1,
  skip.vertex             = FALSE,
  skip.edges              = FALSE,
  skip.arrows             =  !network::is.directed(x),
  add                     = FALSE,
  zero.margins            = TRUE,
  edgelist
) {

  nplot.default(
    x = x,
    layout = layout,
    vertex.size = vertex.size,
    bg.col = bg.col,
    vertex.nsides = vertex.nsides,
    vertex.color = vertex.color,
    vertex.size.range = vertex.size.range,
    vertex.frame.color = vertex.frame.color,
    vertex.rot = vertex.rot,
    vertex.frame.prop = vertex.frame.prop,
    vertex.label = vertex.label,
    vertex.label.fontsize = vertex.label.fontsize,
    vertex.label.color = vertex.label.color,
    vertex.label.fontfamily = vertex.label.fontfamily,
    vertex.label.fontface = vertex.label.fontface,
    vertex.label.show = vertex.label.show,
    vertex.label.range = vertex.label.range,
    edge.width = edge.width,
    edge.width.range = edge.width.range,
    edge.arrow.size = edge.arrow.size,
    edge.color = edge.color,
    edge.curvature = edge.curvature,
    edge.line.lty = edge.line.lty,
    edge.line.breaks = edge.line.breaks,
    sample.edges = sample.edges,
    skip.vertex = skip.vertex,
    skip.edges = skip.edges,
    skip.arrows = skip.arrows,
    add = add,
    zero.margins = zero.margins,
    edgelist = network::as.edgelist(x)
  )

}

#' @export
#' @rdname nplot
#' @importFrom igraph graph_from_adjacency_matrix
nplot.matrix <- function(
  x,
  layout,
  vertex.size             = 1,
  bg.col                  = "transparent",
  vertex.nsides           = 10,
  vertex.color            = grDevices::hcl.colors(1),
  vertex.size.range       = c(.01, .03, 4),
  vertex.frame.color      = NULL,
  vertex.rot              = 0,
  vertex.frame.prop       = .2,
  vertex.label            = NULL,
  vertex.label.fontsize   = NULL,
  vertex.label.color      = adjustcolor("black", alpha.f = .80),
  vertex.label.fontfamily = "sans",
  vertex.label.fontface   = "plain",
  vertex.label.show       = .3,
  vertex.label.range      = c(5, 15),
  edge.width              = 1,
  edge.width.range        = c(1, 2),
  edge.arrow.size         = NULL,
  edge.color              = ~ ego(alpha = .1, col = "gray") + alter,
  edge.curvature          = pi/3,
  edge.line.lty           = "solid",
  edge.line.breaks        = 5,
  sample.edges            = 1,
  skip.vertex             = FALSE,
  skip.edges              = FALSE,
  skip.arrows             = skip.edges,
  add                     = FALSE,
  zero.margins            = TRUE,
  edgelist
  ) {

  x <- igraph::graph_from_adjacency_matrix(x)
  if (missing(layout))
    layout <- igraph::layout_nicely(x)

  nplot.igraph(
    x = x,
    layout = layout,
    vertex.size = vertex.size,
    bg.col = bg.col,
    vertex.nsides = vertex.nsides,
    vertex.color = vertex.color,
    vertex.size.range = vertex.size.range,
    vertex.frame.color = vertex.frame.color,
    vertex.rot = vertex.rot,
    vertex.frame.prop = vertex.frame.prop,
    vertex.label = vertex.label,
    vertex.label.fontsize = vertex.label.fontsize,
    vertex.label.color = vertex.label.color,
    vertex.label.fontfamily = vertex.label.fontfamily,
    vertex.label.fontface = vertex.label.fontface,
    vertex.label.show = vertex.label.show,
    vertex.label.range = vertex.label.range,
    edge.width = edge.width,
    edge.width.range = edge.width.range,
    edge.arrow.size = edge.arrow.size,
    edge.color = edge.color,
    edge.curvature = edge.curvature,
    edge.line.lty = edge.line.lty,
    edge.line.breaks = edge.line.breaks,
    sample.edges = sample.edges,
    skip.vertex = skip.vertex,
    skip.edges = skip.edges,
    skip.arrows = skip.arrows,
    add = add,
    zero.margins = zero.margins,
    edgelist = NULL
  )

}

netplot_theme <- (function() {

  v <- grDevices::hcl.colors(1)
  darkv <- adjustcolor(
    v,
    red.f   = .5,
    green.f = .5,
    blue.f  = .5
  )

  default <- list(
    vertex.core.col   = v,
    vertex.core.fill  = v,
    vertex.frame.col  = darkv,
    vertex.frame.fill = darkv,
    edge.col = ~ego(alpha=0) + alter(alpha = .7),
    name      = "default"
  )

  current <- default

  list(
    set = function(...) {
      dots <- list(...)
      if (!length(dots$name))
        dots$name <- "user"

      current <<- rev(dots)
    },
    get = function() current,
    reset = function() {
      message("Restablishing the default theme")
      current <<- default
    },
    vertex_defaults = function() {
      str(current[grepl("^vertex\\.", names(current))])
    },
    edge_defaults = function() {
      str(current[grepl("^edge\\.", names(current))])
    }
  )

})()

# Function to retrieve the dev.size only if there's an active device
dev_size <- function(...) {

  if (length(grDevices::dev.list())) {

    grDevices::dev.size(...)

  } else
    c(7, 7)

}

#' @export
#' @rdname nplot
#' @param edgelist An edgelist.
#' @return
#' In the case of `nplot.default`, an object of class `netplot` and `grob` (see
#' [grid::grob]) with the following slots:
#'
#' - `children` The main `grob` of the object.
#' - `name` Character scalar. The name of the plot
#' - `.xlim` and `.ylim` Two vectors indicating the limits of the plot
#' - `.layout` A two-column matrix with the location of the vertices.
#' - `.edgelist` A two-column matrix, an edgelist.
#' - `.N` Integer. The number of vertices.
#' - `.M` Integer. The number of edges.
#'
#' The `children` `grob` contains the following two objects:
#'
#' - `background` a `grob` rectangule.
#' - `graph` a `gTree` that contains each vertex and each edge
#' of the figure.
#' @seealso [nplot_base]
nplot.default <- function(
  x,
  layout,
  vertex.size             = 1,
  bg.col                  = "transparent",
  vertex.nsides           = 10,
  vertex.color            = grDevices::hcl.colors(1),
  vertex.size.range       = c(.01, .03, 4),
  vertex.frame.color      = NULL,
  vertex.rot              = 0,
  vertex.frame.prop       = .2,
  vertex.label            = NULL,
  vertex.label.fontsize   = NULL,
  vertex.label.color      = adjustcolor("black", alpha.f = .80),
  vertex.label.fontfamily = "sans",
  vertex.label.fontface   = "plain",
  vertex.label.show       = .3,
  vertex.label.range      = c(5, 15),
  edge.width              = 1,
  edge.width.range        = c(1, 2),
  edge.arrow.size         = NULL,
  edge.color              = ~ ego(alpha = .1, col = "gray") + alter,
  edge.curvature          = pi/3,
  edge.line.lty           = "solid",
  edge.line.breaks        = 5,
  sample.edges            = 1,
  skip.vertex             = FALSE,
  skip.edges              = FALSE,
  skip.arrows             = skip.edges,
  add                     = FALSE,
  zero.margins            = TRUE,
  ...,
  edgelist
  ) {

  # # We turn off the device if not need
  # if (length(grDevices::dev.list()) == 0L) {
  #   on.exit(
  #     grDevices::dev.off(grDevices::dev.cur())
  #   )
  # }

  # listing objects
  netenv <- environment()

  # Checking layout
  if (!inherits(layout, what = c("matrix")))
    stop(
      "-layout- should be of class 'matrix'. It currently is '",
      class(layout),
      "'"
      )

  N <- nrow(layout)
  M <- nrow(edgelist)
  graph_class <- class(x)

  # Mapping attributes ---------------------------------------------------------

  # Nsides
  if (length(vertex.nsides) && inherits(vertex.nsides, "formula")) {

    rhs <- as.character(vertex.nsides[[2]])
    vertex.nsides <- map_attribute_to_shape(
      get_vertex_attribute(graph = x, attribute = rhs)
    )

  }

  # And size
  if (length(vertex.size) && inherits(vertex.size, "formula")) {

    rhs <- as.character(vertex.size[[2]])
    vertex.size <- get_vertex_attribute(graph = x, attribute = rhs)

    # Now check if it is numeric. If not, it should return an error
    if (!is.numeric(vertex.size)) {
      stop("vertex.size must be numeric")
    }

  }

  # Edges width
  if (length(edge.width) && inherits(edge.width, "formula")) {

    rhs <- as.character(edge.width[[2]])
    edge.width <- get_edge_attribute(graph = x, attribute = rhs)

    # Now check if it is numeric. If not, it should return an error
    if (!is.numeric(edge.width)) {
      stop("edge.width must be numeric")
    }

  }

  # Sampling edges -------------------------------------------------------------
  if (sample.edges < 1) {

    sample.edges <- sample.int(
      netenv$M, floor(netenv$M * sample.edges),
      replace = FALSE
      )

    sample.edges <- sort(sample.edges)

    edgelist <- edgelist[sample.edges, , drop=FALSE]

    # Do we need to resize the edgepars?
    epars <- c(
      "edge.width",
      "edge.width.range",
      "edge.arrow.size",
      "edge.color",
      "edge.curvature",
      "edge.line.lty",
      "edge.line.breaks"
      )

    for (epar in epars) {

      if (length(netenv[[epar]]) == netenv$M)
        netenv[[epar]] <- netenv[[epar]][sample.edges]

    }

    netenv$M <- length(sample.edges)

  }

  # end ------------------------------------------------------------------------

  # This function will repeat a patter taking into account the number of columns
  .rep <- function(x, .times) {
    if (grepl("range$", p) | inherits(x, "formula"))
      return(x)
    matrix(rep(x, .times), ncol = length(x), byrow = TRUE)
  }

  # Checking defaults for vertex
  vertex_par <- ls(pattern = "^vertex\\.", envir = netenv)
  for (p in vertex_par)
    if (length(netenv[[p]]) > 0 && length(netenv[[p]]) < netenv$N)
      netenv[[p]] <- .rep(netenv[[p]], netenv$N)

  # Checking defaults for edges
  edge_par <- ls(pattern = "^edge\\.", envir = netenv)
  for (p in edge_par)
    if (length(netenv[[p]]) > 0 && length(netenv[[p]]) < netenv$M)
      netenv[[p]] <- .rep(netenv[[p]], netenv$M)

  # Adjusting size -------------------------------------------------------------

  # Adjusting layout to fit the device
  netenv$layout <- fit_coords_to_dev(netenv$layout)

  # Rescaling size
  if (!skip.vertex) {
    netenv$vertex.size <- rescale_size(
      netenv$vertex.size,
      rel = netenv$vertex.size.range
      )
  } else
    netenv$vertex.size <- rep(0, netenv$N)

  # Rescaling edges
  netenv$edge.width <- rescale_size(
    netenv$edge.width/max(netenv$edge.width, na.rm=TRUE),
    rel = netenv$edge.width.range
    )

  # Rescaling arrows
  if (!length(netenv$edge.arrow.size))
    netenv$edge.arrow.size <- netenv$vertex.size[edgelist[,1]]/1.5

  if (netenv$skip.arrows)
    netenv$edge.arrow.size <- rep(0.0, length(netenv$edge.arrow.size))

  # Rescaling text
  if (!length(netenv$vertex.label.fontsize))
    netenv$vertex.label.fontsize <- rescale_size(
      netenv$vertex.size,
      rel = netenv$vertex.label.range
      )

  # Computing label threshold
  netenv$label_threshold <- stats::quantile(
    netenv$vertex.size, 1 - netenv$vertex.label.show[1])

  # Calculating arrow adjustment
  netenv$arrow.size.adj <- netenv$edge.arrow.size*cos(pi/6)/(
    cos(pi/6) + cos(pi - pi/6 - pi/1.5)
  )/cos(pi/6)


  # Creating viewport with fixed aspectt ratio ----------------------------------
  netenv$xlim <- range(netenv$layout[,1], na.rm=TRUE)
  netenv$ylim <- range(netenv$layout[,2], na.rm=TRUE)

  # Creating layout
  # Solution from this answer https://stackoverflow.com/a/48084527
  asp <- dev_size()

  lo  <- grid::grid.layout(
    widths  = grid::unit(1, "null"),
    heights = grid::unit(asp[2]/asp[1], "null"),
    respect = TRUE # This forcces it to the aspectt ratio
    )

  vp_graph <- grid::viewport(
    xscale         = netenv$xlim + .04*diff(netenv$xlim)*c(-1,1),
    yscale         = netenv$ylim + .04*diff(netenv$ylim)*c(-1,1),
    clip           = "off",
    layout.pos.col = 1,
    layout.pos.row = 1,
    name           = "graph-vp"
  )

  # Generating grobs -----------------------------------------------------------
  if (!skip.vertex) {
    grob.vertex <- vector("list", netenv$N)
    for (v in 1:netenv$N)
      grob.vertex[[v]] <- grob_vertex(netenv, v)
  } else
    grob.vertex <- NULL

  if (!skip.edges || !skip.arrows) {

    grob.edge <- vector("list", netenv$M)
    for (e in 1:netenv$M)
      grob.edge[[e]] <- grob_edge(netenv, e)

  } else
    grob.edge <- NULL
  # Agregated grob -------------------------------------------------------------
  ans <- do.call(
    grid::gTree,
    list(
      children = do.call(grid::gList, c(grob.edge, grob.vertex)),
      name     = "graph",
      vp       = grid::vpTree(
        parent   = grid::viewport(layout = lo, name = "frame-vp"),
        children = grid::vpList(vp_graph)
      )
    )
  )

  # Changing the plotting order
  ans$childrenOrder <- with(
    ans,
    c(
      childrenOrder[grepl("^edge.[0-9]+[-][0-9]+$", childrenOrder)],
      childrenOrder[grepl("^vertex[.][0-9]+$", childrenOrder)]
    ))

  # Adding background
  background <- grid::rectGrob(
    gp = grid::gpar(
      fill = bg.col,
      col  = if (inherits(bg.col, c("GridRadialGradient", "GridPattern"))) {
        grDevices::adjustcolor("gray", red.f = .75, green.f = .75, blue.f = .75)
      } else {
        grDevices::adjustcolor(bg.col, red.f = .75, green.f = .75, blue.f = .75)
      }
    ),
    name = "background",
    vp   = grid::viewport(name = "background-vp")
  )

  ans <- grid::gTree(
    children     = grid::gList(background, ans),
    name         = netplot_name$new(),
    .xlim        = netenv$xlim,
    .ylim        = netenv$ylim,
    .layout      = netenv$layout,
    .edgelist    = netenv$edgelist,
    .N           = netenv$N,
    .M           = netenv$M,
    .graph       = netenv$x,
    .graph_class = netenv$graph_class
  )

  class(ans) <- c("netplot", class(ans))

  # Passing edge color
  if (!skip.vertex && length(vertex.color)) {

    ans <- set_vertex_gpar(
      x       = ans,
      element = "core",
      fill    = vertex.color,
      col     = vertex.color
    )

    vertex.color <- get_vertex_gpar(
      x = ans, element = "core", "fill"
      )$fill

  }

  if (!skip.vertex && !length(vertex.frame.color)) {

    if (inherits(vertex.color, "character")) {

      vertex.frame.color <- grDevices::adjustcolor(
        vertex.color,
        red.f = .75, green.f = .75, blue.f = .75
        )

    } else
      vertex.frame.color <- .rep("darkgray", N)

  }

  if (!skip.vertex)
    ans <- set_vertex_gpar(
      x       = ans,
      element = "frame",
      fill    = vertex.frame.color,
      col     = vertex.frame.color
    )

  if (!skip.edges && length(edge.color)) {

    ans <- set_edge_gpar(x = ans, element = "line", col = edge.color)

    if (!skip.arrows) {

      gp <- get_edge_gpar(x = ans, element = "line", "col", simplify=FALSE)$col
      gp <- sapply(seq_along(gp), function(i) gp[[i]][netenv$edge.line.breaks[i]])
      ans <- set_edge_gpar(x = ans, "arrow", fill = gp, col=gp)

    }
  }

  ans

}

#' Tracker of last printed (plotted) values of `netplot`
#' @noRd
.Last.netplot <- eval({

  env       <- new.env(parent = emptyenv())
  env$value <- NULL

  list(
    get = function() {
      env$value
    },
    set = function(x) {
      env$value <- x
      invisible()
    }
  )

})


#' @rdname nplot
#' @param legend Logical scalar. When `TRUE` it adds a legend.
#' @export
#' @param newpage Logical scalar. When `TRUE` calls [grid::grid.newpage].
#' @param y,... Ignored
print.netplot <- function(x, y = NULL, newpage = TRUE, legend = TRUE, ...) {

  # If legend, then we avoid drawing twice
  if (length(x$.legend_vertex_fill) && legend) {

    color_nodes_legend(x)
    return(invisible(x))

  }

  # Drawing
  if (newpage) {
    grid::grid.newpage()
  }

  grid::grid.draw(x)

  # Storing the value
  .Last.netplot$set(x)

  # Returning
  invisible(x)

}

#' Find a vertex in the current plot
#'
#' This function is a wrapper of [grid::grid.locator()], and provides a way to
#' find the coordinates of a vertex in the current plot. It is useful to
#' identify the vertex that is being clicked in a plot.
#'
#' @param x An object of class `netplot`
#' @return A list with the name of the vertex, the x and y coordinates and the
#'  viewport where it is located.
#' @details This function only works in interactive mode. Once it is called,
#' the user can click on a vertex in the plot. The function will return the
#' name of the vertex, the x and y coordinates and the viewport where it is
#' located. If `x` is not specified, the last plotted `netplot` object will be
#' used.
#'
#' @export
#' @examples
#' library(igraph)
#' library(netplot)
#' set.seed(1)
#' x <- sample_smallworld(1, 200, 5, 0.03)
#'
#' # Plotting
#' nplot(x)
#'
#' # Clicking (only works in interactive mode)
#' if (interactive()) {
#'  res <- locate_vertex()
#'  print(res)
#' }
#'
locate_vertex <- function(x = NULL) {

  if (is.null(x))
    x <- .Last.netplot$get()

  # Trying to fecth the
  on.exit(grid::upViewport(0L))
  res <- tryCatch(grid::downViewport("graph-vp"), error = function(e) e)

  if (inherits(res, "error"))
    stop("Couldn't find the network region. Perhaps there's none?", call. = FALSE)

  loc <- grid::grid.locator()
  loc <- as.vector(unlist(loc))
  loc <- matrix(loc, ncol = 2, nrow = x$.N, byrow = TRUE)

  # Which is the closests one
  v <- which.min(abs(x$.layout - loc))

  list(
    name = sprintf("vertex.%i", v),
    x    = loc[1,1],
    y    = loc[1,2],
    vp   = grid::current.viewport()
    )

}


# Look at `chull` from `grDevices`


