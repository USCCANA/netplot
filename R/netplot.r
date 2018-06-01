#' A wrapper of `colorRamp2`
#' @param i,j Integer scalar. Indices of ego and alter from 1 through n.
#' @param p Numeric scalar from 0 to 1. Proportion of mixing.
#' @param vcols Vector of colors.
#' @param alpha Numeric scalar from 0 to 1. Passed to [polygons::colorRamp2]
#' @return A color.
edge_color_mixer <- function(i, j, vcols, p = .5, alpha = .15) {

  grDevices::adjustcolor(grDevices::rgb(
    polygons::colorRamp2(vcols[c(i,j)], alpha = FALSE)(p),
    maxColorValue = 255
  ), alpha = alpha)

}

#' Plot a network
#'
#' @param x An `igraph` object.
#' @param bg.col Color of the background.
#' @param layout Numeric two-column matrix with the graph layout.
#' @param vertex.size Numeric vector of length `vcount(x)`. Absolute size of the vertex.
#' @param vertex.nsides Numeric vector of length `vcount(x)`. Number of sizes of
#' the vertex. E.g. three is a triangle, and 100 approximates a circle.
#' @param vertex.size.range Vector of length `vcount(x)`.
#' @param vertex.frame.prop Vector of length `vcount(x)`. What proportion of the
#' vertex does the frame occupy (values between 0 and 1).
#' @param vertex.rot Vector of length `vcount(x)`. Passed to [polygons::npolygon],
#' elevation degree from which the polygon is drawn.
#' @param vertex.label Character vector of length `vcount(x)`. Labels.
#' @param vertex.label.show Numeric scalar. Proportion of labels to show as the
#' top ranking according to `vertex.size`.
#' @param vertex.label.range Numeric vector of size 2 or 3. Relative scale of
#' `vertex.label.fontsize` in points (see [grid::gpar]).
#' @param edge.width.range Vector of length `ecount(x)`.
#' @param edge.arrow.size Vector of length `ecount(x)`.
#' @param edge.curvature Numeric vector of length `ecount(x)`. Curvature of edges
#' in terms of radians.
#' @param edge.line.breaks Vector of length `ecount(x)`. Number of vertices to
#' draw (approximate) the arc (edge).
#' @param sample.edges Numeric scalar between 0 and 1. Proportion of edges to sample.
#' @param skip.vertex,skip.edges,skip.arrows Logical scalar. When `TRUE` the object
#' is not plotted.
#' @param add Logical scalar.
#' @param vertex.col,vertex.fill Color (border) and fill of the vertex (both core
#' and frame).
#' @param edge.line.lwd Passed to [set_gpar()].
#' @param ... Further arguments passed to [set_gpar()].
#' @param gpar_order Vector of character specifying in what other the graphical
#' parameters passed via `...` are evaluated.
#' @param zero.margins Logical scalar.
#' @importFrom viridis viridis
#' @importFrom igraph layout_with_fr degree vcount ecount
#' @importFrom grDevices adjustcolor rgb
#' @importFrom graphics lines par plot polygon rect segments plot.new plot.window
#' @importFrom polygons piechart npolygon rotate colorRamp2 segments_gradient
#'
#' @details
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
nplot <- function(...) UseMethod("nplot")

#' @export
#' @rdname nplot
nplot.igraph <- function(
  x,
  layout        = igraph::layout_nicely(x),
  vertex.size   = igraph::degree(x, mode="in"),
  vertex.col    = "tomato",
  vertex.label  = igraph::vertex_attr(x, "name"),
  edge.line.lwd = igraph::edge_attr(x, "weight"),
  ...
  ) {

  if (!length(edge.line.lwd))
    edge.line.lwd <- 1L

  nplot.default(
    edgelist      = igraph::as_edgelist(x, names = FALSE),
    layout        = layout,
    vertex.size   = vertex.size,
    vertex.col    = vertex.col,
    vertex.label  = vertex.label,
    edge.line.lwd = edge.line.lwd,
    ...
  )

}

#' @export
#' @rdname nplot
#' @importFrom network as.edgelist
#' @importFrom sna gplot.layout.kamadakawai
nplot.network <- function(
  x,
  layout       = sna::gplot.layout.kamadakawai(x, NULL),
  vertex.size  = sna::degree(x, cmode="indegree"),
  vertex.col = "tomato",
  vertex.label = network::get.vertex.attribute(x, "vertex.names"),
  ...
) {

  nplot.default(
    edgelist    = network::as.edgelist(x),
    layout      = layout,
    vertex.size = vertex.size,
    vertex.col = vertex.col,
    vertex.label = vertex.label,
    ...
  )

}

netplot_theme <- (function() {

  v <- viridis::viridis(1)
  darkv <- adjustcolor(
    viridis::viridis(1),
    red.f   = .7,
    green.f = .7,
    blue.f  = .7
  )

  default <- list(
    vertex.core.col  = v,
    vertex.core.fill = v,
    vertex.frame.col = darkv,
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
    }
  )

})()


# vertex.color            = viridis::viridis(1),
# vertex.frame.color      = grDevices::adjustcolor(vertex.color, red.f = .75, green.f = .75, blue.f = .75),
# vertex.label.fontsize   = NULL,
# vertex.label.color      = "black",
# vertex.label.fontfamily = "HersheySans",
# vertex.label.fontface   = "bold",
# vertex.label.show       = .3,
# edge.width              = 1,
# edge.color              = ~ ego(alpha = .01) + alter,
# edge.line.lty           = "solid",

#' @export
#' @rdname nplot
#' @param edgelist An edgelist.
nplot.default <- function(
  edgelist,
  layout,
  vertex.size             = 1,
  bg.col                  = "transparent",
  vertex.nsides           = 50,
  vertex.size.range       = c(.01, .03),
  vertex.rot              = 0,
  vertex.frame.prop       = .2,
  vertex.label            = NULL,
  vertex.label.range      = c(5, 15),
  vertex.label.show       = 1,
  edge.width.range        = c(1, 2),
  edge.arrow.size         = NULL,
  edge.curvature          = pi/3,
  edge.line.breaks        = 15,
  sample.edges            = 1,
  skip.vertex           = FALSE,
  skip.edges              = FALSE,
  skip.arrows             = skip.edges,
  add                     = FALSE,
  zero.margins            = TRUE,
  gpar_order              = c("vertex", "vertex.core", "vertex.frame", "vertex.label", "edge", "edge.line", "edge.arrow"),
  ...
  ) {


  # listing objects
  netenv <- as.environment(mget(ls()))
  netenv$gpar <- parse_gpar(...)

  netenv$N <- nrow(layout)
  netenv$M <- nrow(edgelist)

  # This function will repeat a patter taking into account the number of columns
  .rep <- function(x, .times) {
    if (grepl("range$", p) | inherits(x, "formula"))
      return(x)
    matrix(rep(x, .times), ncol = length(x), byrow = TRUE)
  }

  # Checking defaults for vertex
  for (p in ls(pattern = "^vertex", envir = netenv))
    if (length(netenv[[p]]) > 0 && length(netenv[[p]]) < netenv$N)
      netenv[[p]] <- .rep(netenv[[p]], netenv$N)

  # Checking defaults for edges
  for (p in ls(pattern = "^edge", envir = netenv))
    if (length(netenv[[p]]) > 0 && length(netenv[[p]]) < netenv$M)
      netenv[[p]] <- .rep(netenv[[p]], netenv$M)


  # Adjusting size -------------------------------------------------------------

  # Adjusting layout to fit the device
  netenv$layout <- fit_coords_to_dev(netenv$layout)

  # Rescaling size
  netenv$vertex.size <- rescale_size(
    netenv$vertex.size,
    rel = netenv$vertex.size.range
    )

  # Rescaling edges
  # netenv$edge.line.lwd <- rescale_size(
  #   netenv$edge.line.lwd/max(netenv$edge.line.lwd, na.rm=TRUE),
  #   rel = netenv$edge.width.range
  #   )

  # Rescaling arrows
  if (!length(netenv$edge.arrow.size))
    netenv$edge.arrow.size <- netenv$vertex.size[edgelist[,1]]/1.5

  # Rescaling text
  if (length(netenv$vertex.label))
    if (!length(netenv$gpar$vertex_label)) {

      netenv$gpar <- c(netenv$gpar,
      parse_gpar(
        vertex.label.fontsize = rescale_size(
          netenv$vertex.size,
          rel = netenv$vertex.label.range
        )
        ))

    }



  # Computing label threshold
  netenv$label_threshold <- stats::quantile(
    netenv$vertex.size, 1 - netenv$vertex.label.show[1])

  # Calculating arrow adjustment
  netenv$arrow.size.adj <- netenv$edge.arrow.size*cos(pi/6)/(
    cos(pi/6) + cos(pi - pi/6 - pi/1.5)
  )/cos(pi/6)


  # Creating viewport with fixed aspect ratio ----------------------------------
  netenv$xlim <- range(netenv$layout[,1], na.rm=TRUE)
  netenv$ylim <- range(netenv$layout[,2], na.rm=TRUE)


  # Creating layout
  # Solution from this answer https://stackoverflow.com/a/48084527
  asp <- grDevices::dev.size()
  lo  <- grid::grid.layout(
    widths  = grid::unit(1, "null"),
    heights = grid::unit(asp[2]/asp[1], "null"),
    respect = TRUE # This forcces it to the aspect ratio
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
  grob.vertex <- vector("list", netenv$N)
  for (v in 1:netenv$N)
    grob.vertex[[v]] <- grob_vertex(netenv, v)

  grob.edge <- vector("list", netenv$M)
  for (e in 1:netenv$M)
    grob.edge[[e]] <- grob_edge(netenv, e)

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
      col  = grDevices::adjustcolor(bg.col, red.f = .75, green.f = .75, blue.f = .75)
    ),
    name = "background",
    vp   = grid::viewport(name = "background-vp")
  )

  ans <- grid::gTree(
    children  = grid::gList(background, ans),
    name      = netplot_name$new(),
    .xlim     = netenv$xlim,
    .ylim     = netenv$ylim,
    .layout   = netenv$layout,
    .edgelist = netenv$edgelist,
    .N        = netenv$N,
    .M        = netenv$M
  )

  class(ans) <- c("netplot", class(ans))

  # Setting gpar
  for (g in gpar_order)
    if (g %in% names(netenv$gpar))
      ans <- do.call(set_gpar, c(list(ans), netenv$gpar[[g]]))

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
#' @export
#' @param newpage Logical scalar. When `TRUE` calls [grid::grid.newpage].
#' @param y,... Ignored
print.netplot <- function(x, y = NULL, newpage=TRUE, ...) {

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


locate_vertex <- function(x) {

  # Trying to fecth the
  on.exit(grid::upViewport(0L))
  res <- tryCatch(grid::downViewport("graph-vp"), error = function(e) e)

  if (inherits(res, "error"))
    stop("Couldn't find the network region. Perhaps there's none?", call. = FALSE)

  loc <- grid::grid.locator()
  loc <- as.vector(unlist(loc))
  loc <- matrix(loc, ncol = 2, nrow = nrow(x$layout), byrow = TRUE)

  # Which is the closests one
  v <- which.min(abs(x$layout - loc))

  list(
    name = paste0("vertex.", v),
    x    = loc[1,1],
    y    = loc[1,2],
    vp   = grid::current.viewport()
    )

}


# Look at `chull` from `grDevices`


