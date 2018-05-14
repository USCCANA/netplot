

#' Arc between two nodes
#'
#' @param p0,p1 Numeric vector of length 2. Center coordinates
#' @param alpha Numeric scalar. Arc angle in radians.
#' @param n Integer scalar. Number of segments to approximate the arc.
#' @param radii Numeric vector of length 2. Radious
#' @export
#'
arc <- function(
  p0,
  p1,
  alpha = pi/3,
  n     = 20L,
  radii = c(0, 0)
) {

  # If no curve, nothing to do (old fashioned straight line)
  if (alpha == 0) {
    alpha <- 1e-5
  }

  elevation <- atan2(p1[2]-p0[2], p1[1] - p0[1])

  # Constants
  d <- stats::dist(rbind(p0, p1))

  # If overlapping, then fix the radius to be the average
  if ((d - sum(radii)) < 0) {
    r <- mean(radii)
    alpha <- 2*pi - asin(d/2/r)*2
  } else {
    r <- d/2/(sin(alpha/2))
  }


  # Angles
  alpha0 <- asin(radii[1]/2/r)*2
  alpha1 <- asin(radii[2]/2/r)*2

  # Angle range
  alpha_i <- seq(
    pi/2 + (alpha/2 - alpha0) ,
    pi/2 - (alpha/2 - alpha1),
    length.out = n + 1
  )

  # Middle point
  M <- c(
    p0[1] + d/2,
    p0[2] - cos(alpha/2)*r
  )

  ans <- cbind(
    M[1] + cos(alpha_i)*r,
    M[2] + sin(alpha_i)*r
  )


  # Rotation and return
  ans <- polygons::rotate(ans, p0, elevation)

  # Separating the segments
  ans <- cbind(
    as.vector(t(cbind(ans[-(n + 1),1], ans[-1,1]))),
    as.vector(t(cbind(ans[-(n + 1),2], ans[-1,2])))
  )

  structure(
    ans,
    alpha0 = atan2(ans[1,2] - p0[2], ans[1,1] - p0[1]),
    alpha1 = atan2(p1[2] - ans[n*2,2], p1[1] - ans[n*2,1]),
    midpoint = ans[ceiling(n/2),]
  )

}


#' Arrow polygon.
#'
#' @param x Numeric vector of length 2. Coordinates of the tip
#' @param alpha,l,a,b Numeric scalars
#' @export
arrow_fancy <- function(x, alpha = 0, l=.25, a=pi/6, b = pi/1.5) {


  p_top   <- c(x[1], x[2])
  p_left  <- p_top + c(-cos(a), sin(a))*l

  base <- l*sin(a)
  base2 <- base * cos(pi - b)/sin(pi - b)
  p_mid   <- p_left + c(base2, -base)

  p_right <- p_top - c(cos(a), sin(a))*l

  ans <- rbind(p_top, p_left, p_mid, p_right)

  # Rotation

  polygons::rotate(ans, p_top, alpha = alpha)


}

#' Rescale the size of a node to make it relative to the aspect ratio of the device
#' @param size Numeric vector. Size of the node (radious).
#' @param rel Numeric vector of length 3. Relative size for the minimum and maximum
#' of the plot, and curvature of the scale. The third number is used as `size^rel[3]`.
#'
#' @details
#' This function is to be called after [plot.new], as it takes the parameter `usr`
#' from the
rescale_node <- function(size, rel=c(.01, .05, 1)) {

  # Checking the rel size
  if (length(rel) == 2)
    rel <- c(rel, 1)
  else if (length(rel) > 3) {
    warning("`rel` has more than 3 elements. Only the first 3 will be used.")
  } else if (length(rel) < 2) {
    stop("`rel` must be at least of length 2 and at most of length 3.")
  }


  # Creating curvature
  size <- size^rel[3]

  # Rescaling to be between range[1], range[2]
  sran <- range(size, na.rm=TRUE)

  if ((sran[2] - sran[1]) > 1e-10)
    size <- (size - sran[1])/(sran[2] - sran[1]) # 0-1
  else
    size <- size/sran[1]

  size <- size * (rel[2] - rel[1]) + rel[1]

  # Getting coords
  # usr <- graphics::par()$usr[1:2]
  # size * (usr[2] - usr[1])/2
  size

}

#' Function to rescale the edge-width.
#' @param rel Vector of length 2 with the min and max width.
#' @param width Numeric vector. width of the edges.
#' @export
rescale_edge <- function(width, rel=c(1, 3, 1)) {

  # Checking the rel size
  if (length(rel) == 2)
    rel <- c(rel, 1)
  else if (length(rel) > 3) {
    warning("`rel` has more than 3 elements. Only the first 3 will be used.")
  } else if (length(rel) < 2) {
    stop("`rel` must be at least of length 2 and at most of length 3.")
  }

  # Creating curvature
  width <- width^rel[3]

  ran   <- range(width, na.rm = TRUE)
  if (ran[1] != ran[2])
    width <- (width - ran[1])/(ran[2] - ran[1] + 1e-10)
  else
    width <- width/ran[1]

  width*(rel[2] - rel[1]) + rel[1]

}


#' Adjust coordinates to fit aspect ratio of the device
#' @param coords Two column numeric matrix. Vertices coordinates.
#' @details
#' It first adjusts `coords` to range between `-1,1`, and then, using
#' `graphics::par("pin")`, it rescales the second column of it (`y`) to adjust
#' for the device's aspec ratio.
#' @param adj Numeric vector of length 2.
#' @export
fit_coords_to_dev <- function(coords, adj = grDevices::dev.size()) {

  # Making it -1 to 1
  yran <- range(coords[,2], na.rm = TRUE)
  xran <- range(coords[,1], na.rm = TRUE)

  coords[,1] <- (coords[,1] - xran[1])/(xran[2] - xran[1])*2 - 1
  coords[,2] <- (coords[,2] - yran[1])/(yran[2] - yran[1])*2 - 1

  # Adjusting aspect ratio according to the ploting area
  coords[,2] <- coords[,2]*adj[2]/adj[1]

  # Returning new coordinates
  coords

}


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
#' @param vertex.shape Numeric vector of length `vcount(x)`. Number of sizes of
#' the vertex. E.g. three is a triangle, and 100 approximates a circle.
#' @param vertex.color Vector of length `vcount(x)`. Vertex colors.
#' @param vertex.size.range Vector of length `vcount(x)`.
#' @param vertex.frame.color Vector of length `vcount(x)`.
#' @param vertex.frame.prop Vector of length `vcount(x)`. What proportion of the
#' vertex does the frame occupy (values between 0 and 1).
#' @param vertex.shape.degree Vector of length `vcount(x)`. Passed to [polygons::npolygon],
#' elevation degree from which the polygon is drawn.
#' @param edge.width Vector of length `ecount(x)`.
#' @param edge.width.range Vector of length `ecount(x)`.
#' @param edge.arrow.size Vector of length `ecount(x)`.
#' @param edge.curvature Numeric vector of length `ecount(x)`. Curvature of edges
#' in terms of radians.
#' @param edge.color.mix Numeric vector of length `ecount(x)` with values in
#' `[0,1]`. 0 means color equal to ego's vertex color, one equals to alter's
#' vertex color.
#' @param edge.color.alpha Either a vector of length 1 or 2, or a matrix of
#' size `ecount(x)*2` with values in `[0,1]`. Alpha (transparency) levels (see
#' details)
#' @param edge.line.lty Vector of length `ecount(x)`.
#' @param edge.line.breaks Vector of length `ecount(x)`. Number of vertices to
#' draw (approximate) the arc (edge).
#' @param sample.edges Numeric scalar between 0 and 1. Proportion of edges to sample.
#' @param skip.vertices,skip.edges,skip.arrows Logical scalar. When `TRUE` the object
#' is not plotted.
#' @param add Logical scalar.
#' @param zero.margins Logical scalar.
#' @importFrom viridis viridis
#' @importFrom igraph layout_with_fr degree vcount ecount
#' @importFrom grDevices adjustcolor rgb
#' @importFrom graphics lines par plot polygon rect segments plot.new plot.window
#' @importFrom polygons piechart npolygon rotate colorRamp2 segments_gradient
#'
#' @details
#' In the case of `edge.color.alpha`, the user can specify up to 2 alpha levels
#' per edge. Edges are drawn using [polygons::segments_gradient] which allows
#' drawing segments with a color gradient. In this case setting, for example
#' `c(.1, .5)` will make the edge start with a transparency of 0.1 and end
#' with 0.5.
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
NULL

nplot_obj <- function(
  x,
  layout,
  vertex.size,
  vertes.size.range,
  vertex.shape,
  vertex.color,
  vertex.frame.color,
  vertex.frame.prop,
  edge.width,
  edge.width.range,
  edge.color,
  edge.color.type, # = c("mix", "plain")
  edge.color.alpha,
  edge.curvature,
  edge.line.lty,
  edge.breaks
) {



}

#' List of valid gp parameters
#' @noRd
gparArgs <- c("col",
  "fill",
  "alpha",
  "lty",
  "lwd",
  "lex",
  "lineend",
  "linejoin",
  "linemitre",
  "fontsize",
  "cex",
  "fontfamily",
  "fontface",
  "lineheight",
  "font"
  )


#' Compute coordinates for an nsided polygon
#' @noRd
npolygon <- function (x = 0, y = 0, n = 6L, r = 1, d = 2 * pi/(n)/2) {
  deg <- seq(d, 2 * pi + d, length.out = n + 1)[-(n + 1)]
  cbind(x = cos(deg) * r + x, y = sin(deg) * r + y)
}


#' @export
nplot2 <- function(...) UseMethod("nplot2")

#' @export
nplot2.igraph <- function(
  x,
  layout = igraph::layout_nicely(x),
  ...
  ) {

  nplot2.default(
    edgelist = igraph::as_edgelist(x),
    layout   = layout,
    ...
  )

}

#' @export
nplot2.default <- function(
  edgelist,
  layout,
  vertex.size         = 1,
  bg.col              = "lightgray",
  vertex.nsides       = 50,
  vertex.color        = viridis::viridis(1),
  vertex.size.range   = c(.01, .03),
  vertex.frame.color  = grDevices::adjustcolor(vertex.color, red.f = 1.5, green.f = 1.5, blue.f = 1.5),
  vertex.shape.degree = 0,
  vertex.frame.prop   = .1,
  edge.width          = 1,
  edge.width.range    = c(1, 2),
  edge.arrow.size     = NULL,
  edge.color.mix      = .5,
  edge.color.alpha    = c(.1, .5),
  edge.curvature      = pi/3,
  edge.line.lty       = "solid",
  edge.line.breaks    = 15,
  sample.edges        = 1,
  skip.vertices       = FALSE,
  skip.edges          = FALSE,
  skip.arrows         = skip.edges,
  add                 = FALSE,
  zero.margins        = TRUE,
  ...
  ) {


  # listing objects
  netenv <- as.environment(mget(ls()))

  netenv$N <- nrow(layout)
  netenv$M <- nrow(edgelist)

  # This function will repeat a patter taking into account the number of columns
  .rep <- function(x, .times) {
    if (grepl("range$", p))
      return(x)
    matrix(rep(x, .times), ncol = length(x), byrow = TRUE)
  }

  # Checking defaults for vertex
  for (p in ls(pattern = "^vertex", envir = netenv))
    if (length(netenv[[p]]) > 0 && length(netenv[[p]]) < netenv$N)
      netenv[[p]] <- .rep(netenv[[p]], netenv$N)

  # Checking defaults for edges
  for (p in ls(patter = "^edge", envir = netenv))
    if (length(netenv[[p]]) > 0 && length(netenv[[p]]) < netenv$M)
      netenv[[p]] <- .rep(netenv[[p]], netenv$M)


  # Adjusting size -------------------------------------------------------------

  # Adjusting layout to fit the device
  netenv$layout <- fit_coords_to_dev(netenv$layout)

  # Rescaling size
  netenv$vertex.size <- rescale_node(
    netenv$vertex.size,
    rel = netenv$vertex.size.range
    )

  # Rescaling edges
  netenv$edge.width <- rescale_edge(
    netenv$edge.width/max(netenv$edge.width, na.rm=TRUE),
    rel = netenv$edge.width.range
    )

  # Rescaling arrows
  if (!length(netenv$edge.arrow.size))
    netenv$edge.arrow.size <- netenv$vertex.size[edgelist[,1]]/1.5

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
  for (v in 1:netenv$N)
    grob_vertex(netenv, v)

  for (e in 1:netenv$M)
    grob_edge(netenv, e)

  # Agregated grob -------------------------------------------------------------
  netenv$grob <- do.call(
    grid::gTree,
    list(
      children = do.call(grid::gList, c(netenv$grob.edge, netenv$grob.vertex)),
      name     = "graph",
      vp       = grid::vpTree(
        parent   = grid::viewport(layout = lo, name = "frame-vp"),
        children = grid::vpList(vp_graph)
      )
    )
  )

  netenv


}

#' Functions to calculate graph polygons coordinates
#' @param netenv An object of class network environment.
#' @param v,e Integer scalars. vertex or edge index.
#'
#' @return
# Generating coordinates
grob_vertex <- function(netenv, v) {

  # Computing coordinates
  coords <- npolygon(
      x = netenv$layout[v, 1],
      y = netenv$layout[v, 2],
      n = netenv$vertex.nsides[v],
      r = netenv$vertex.size[v]*(1 - netenv$vertex.frame.prop[v])
    )

  # Frame coordinates
  framecoords <- npolygon(
      x = netenv$layout[v, 1],
      y = netenv$layout[v, 2],
      n = netenv$vertex.nsides[v],
      r = netenv$vertex.size[v]
      )

  # If the list is empty
  if (!length(netenv$grob.vertex))
    netenv$grob.vertex <- vector("list", netenv$N)

  # Returning
  netenv$grob.vertex[[v]] <-
    grid::polygonGrob(
      x    = c(framecoords[,1], coords[,1]),
      y    = c(framecoords[,2], coords[,2]),
      id.lengths = c(nrow(coords), nrow(framecoords)),
      gp   = grid::gpar(
        fill = c(netenv$vertex.color[v], netenv$vertex.frame.color[v]),
        col  = c(netenv$vertex.color[v], netenv$vertex.frame.color[v])
      ),
      default.units = "native",
      name = paste0("vertex.", v)
      )

  invisible(NULL)
}

#' Computes edges
#' @noRd
grob_edge <- function(netenv, e) {

  # Obtaining positions of ego and alter
  i       <- netenv$edgelist[e, 1]
  j       <- netenv$edgelist[e, 2]
  nbreaks <- netenv$edge.line.breaks[e]

  # Computing coordinates
  coords <- arc(
    p0    = netenv$layout[i,],
    p1    = netenv$layout[j,],
    radii = netenv$vertex.size[c(i,j)] + c(0, netenv$arrow.size.adj[e]),
    alpha = netenv$edge.curvature[e],
    n     = nbreaks
  )

  # If the list is empty
  if (!length(netenv$grob.edge))
    netenv$grob.edge <- vector("list", netenv$M)

  # Computing colors using colorRamp2
  col <- polygons::colorRamp2(c(netenv$vertex.color[i], netenv$vertex.color[j]))(.5)
  col <- rgb(col, maxColorValue = 255)

  col <- polygons::colorRamp2(
    c(
      adjustcolor(col, alpha.f = netenv$edge.color.alpha[e,1]),
      adjustcolor(col, alpha.f = netenv$edge.color.alpha[e,2])
    ), alpha = TRUE)

  col <- col(seq(0,1, length.out = nbreaks))
  col <- grDevices::rgb(col, alpha = col[,4], maxColorValue = 255)


  # Generating grob
  netenv$grob.edge[[e]] <- grid::polylineGrob(
    x          = coords[,1],
    y          = coords[,2],
    id.lengths = rep(2, nbreaks),
    default.units = "native",
    gp         = grid::gpar(
      col = c(col),
      lty = rep(netenv$edge.line.lty[e], nbreaks),
      lwd = rep(netenv$edge.width[e], nbreaks),
      lineend = rep(1, nbreaks)
    ),
    name = "line"
    )

  # Arrow
  # Computing arrow
  alpha1 <- attr(coords, "alpha1")
  arrow  <- arrow_fancy(
    x     = coords[nbreaks*2, 1:2] +
      netenv$arrow.size.adj[e]*c(cos(alpha1), sin(alpha1)),
    alpha = alpha1,
    l     = netenv$edge.arrow.size[e]
  )

  netenv$grob.edge[[e]] <- grid::grobTree(
    netenv$grob.edge[[e]],
    grid::polygonGrob(
      arrow[,1],
      arrow[,2],
      default.units = "native",
      name = "arrow",
      gp   = grid::gpar(
        col  = col[nbreaks],
        fill = col[nbreaks],
        lwd  = netenv$edges.width[e]
      )
    ),
    name = paste0("edge", i, "-",j)
    )

  invisible(NULL)

}

#' Look at `chull` from `grDevices`
#' @noRd

#
# coords_calc_edge <- function(netenv, e) {
#
#
#
# }
#
# coords_calc_arrow <- function(netenv, e) {
#
#
#
# }


#' @rdname nplot
#' @export
nplot <- function(
  x,
  layout              = igraph::layout_nicely(x),
  vertex.size         = igraph::degree(x, mode="in"),
  bg.col              = "lightgray",
  vertex.shape        = 50,
  vertex.color        = NULL,
  vertex.size.range   = c(.01, .03),
  vertex.frame.color  = NULL,
  vertex.shape.degree = 0,
  vertex.frame.prop   = .1,
  edge.width          = NULL,
  edge.width.range    = c(1, 2),
  edge.arrow.size     = NULL,
  edge.color.mix      = .5,
  edge.color.alpha    = c(.1, .5),
  edge.curvature      = pi/3,
  edge.line.lty       = "solid",
  edge.line.breaks    = 15,
  sample.edges        = 1,
  skip.vertices       = FALSE,
  skip.edges          = FALSE,
  skip.arrows         = skip.edges,
  add                 = FALSE,
  zero.margins        = TRUE
) {

  # Computing colors
  if (!length(vertex.color)) {
    vertex.color <- length(table(igraph::degree(x, mode = "in")))
    vertex.color <- viridis::viridis(vertex.color)
    vertex.color <- vertex.color[
      as.factor(igraph::degree(x, mode = "in"))
      ]
  }

  # # Creating the window
  if (zero.margins) {
    oldpar <- graphics::par(mai=rep(0, 4))
    on.exit(graphics::par(oldpar))
  }

  if (!add)
    graphics::plot.new()

  # Adjusting layout to fit the device
  layout <- fit_coords_to_dev(layout)

  # Plotting
  xlim <- range(layout[,1])
  ylim <- range(layout[,2])
  graphics::plot.window(xlim, ylim, asp=1, new=FALSE)

  # Adding rectangle
  if (length(bg.col)) {
    usr <- graphics::par("usr")
    rect(
      usr[1], usr[3], usr[2], usr[4],
      col = bg.col,
      border = grDevices::adjustcolor(bg.col, red.f = 1.5, green.f = 1.5, blue.f = 1.5))
  }

  # Rescaling size
  vertex.size <- rescale_node(vertex.size, rel = vertex.size.range)

  # Computing shapes -----------------------------------------------------------
  E <- igraph::as_edgelist(x, names = FALSE)

  if (sample.edges < 1) {
    sample.edges <- sample.int(nrow(E), floor(nrow(E)*sample.edges))
    E <- E[sample.edges, , drop=FALSE]
  }

  # Weights
  if (!length(edge.width))
    edge.width <- rep(1.0, igraph::ecount(x))

  # Rescaling edges
  edge.width <- rescale_edge(edge.width/max(edge.width, na.rm=TRUE), rel = edge.width.range)

  if (!length(edge.arrow.size))
    edge.arrow.size <- vertex.size[E[,1]]/1.5
  else if (length(edge.arrow.size) == 1L)
    edge.arrow.size <- rep(edge.arrow.size, nrow(E))

  # Calculating arrow adjustment
  arrow.size.adj <- edge.arrow.size*cos(pi/6)/(
    cos(pi/6) + cos(pi - pi/6 - pi/1.5)
  )/cos(pi/6)

  # Making space for the arrow and edges coordinates
  edges.coords <- vector("list", nrow(E))
  edges.arrow.coords <- vector("list", length(edges.coords))

  if (length(edge.curvature) == 1)
    edge.curvature <- rep(edge.curvature, length(edges.coords))

  if (length(edge.line.breaks) == 1)
    edge.line.breaks <- rep(edge.line.breaks, length(edges.coords))


  for (e in 1:nrow(E)) {

    i <- E[e,1]
    j <- E[e,2]

    # Calculating edges coordinates
    edges.coords[[e]] <- arc(
      layout[i,], layout[j,],
      radii = vertex.size[c(i,j)] + c(0, arrow.size.adj[e]),
      alpha = edge.curvature[e],
      n     = edge.line.breaks[e]
    )

    # Computing arrow
    alpha1 <- attr(edges.coords[[e]], "alpha1")
    edges.arrow.coords[[e]] <- arrow_fancy(
      x = edges.coords[[e]][nrow(edges.coords[[e]]),1:2] +
        arrow.size.adj[e]*c(cos(alpha1), sin(alpha1)),
      alpha = alpha1,
      l     = edge.arrow.size[e]
    )

  }

  # Edges
  if (!length(edge.color.mix))
    edge.color.mix <- rep(.5, length(edges.coords))
  else if (length(edge.color.mix) == 1)
    edge.color.mix <- rep(edge.color.mix, length(edges.coords))

  if (!length(edge.line.lty))
    edge.line.lty <- rep(1L, length(edges.coords))
  else if (length(edge.line.lty) == 1)
    edge.line.lty <- rep(edge.line.lty, length(edges.coords))

  if (!length(edge.color.alpha))
    edge.color.alpha <- matrix(.5, nrow= length(edges.coords), ncol=2)
  else if (length(edge.color.alpha) <= 2)
    edge.color.alpha <- matrix(edge.color.alpha, nrow= length(edges.coords), ncol=2, byrow = TRUE)

  # Nodes
  if (length(vertex.color) == 1)
    vertex.color <- rep(vertex.color, nrow(layout))

  if (!length(vertex.frame.color))
    vertex.frame.color <- adjustcolor(vertex.color, red.f = .75, blue.f = .75, green.f = .75)

  if (length(vertex.shape) == 1)
    vertex.shape <- rep(vertex.shape, nrow(layout))

  if (length(vertex.shape.degree) == 1)
    vertex.shape.degree <- rep(vertex.shape.degree, nrow(layout))

  if (length(vertex.frame.prop) == 1)
    vertex.frame.prop <- rep(vertex.frame.prop, nrow(layout))

  edges.color <- vector("list", length(edges.coords))

  for (i in seq_along(edges.coords)) {

    if (!length(edges.coords[[i]]))
      next

    # Not plotting self (for now)
    if (E[i,1] == E[i, 2])
      next

    # Computing edge color
    col <- edge_color_mixer(
      i     = E[i, 1],
      j     = E[i, 2],
      vcols = vertex.color,
      p     = edge.color.mix[i],
      alpha = 1
    )

    edges.color[[i]] <- polygons::colorRamp2(
      c(
        adjustcolor(col, alpha.f = edge.color.alpha[i,1]),
        adjustcolor(col, alpha.f = edge.color.alpha[i,2])
      ), alpha = TRUE)

    # Drawing lines
    if (!skip.edges) {
      polygons::segments_gradient(
        edges.coords[[i]], lwd= edge.width[i],
        col = edges.color[[i]],
        lty = edge.line.lty[i]
      )
    }

    if (!skip.arrows) {
      # Drawing arrows
      graphics::polygon(
        edges.arrow.coords[[i]],
        col    = col,
        border = col,
        lwd    = edge.width[i]
      )
    }

  }

  vertex.frame.prop <- 1 - vertex.frame.prop

  vertex.coords <- vector("list", nrow(layout))
  vertex.frame.coords <- vector("list", nrow(layout))

  if (!skip.vertices)
    for (i in 1:nrow(layout)) {
      # Computing coordinates
      vertex.coords[[i]] <- polygons::npolygon(
        layout[i,1], layout[i,2],
        n = vertex.shape[i],
        r = vertex.size[i]*vertex.frame.prop[i],
        vertex.shape.degree[i]
      )
      vertex.frame.coords[[i]] <- polygons::piechart(
        1,
        origin = layout[i,],
        edges  = vertex.shape[i],
        radius = vertex.size[i],
        doughnut = vertex.size[i]*vertex.frame.prop[i],
        rescale = FALSE,
        add     = TRUE,
        skip.plot.slices = TRUE
      )$slices[[1]]


      # Circle
      graphics::polygon(
        vertex.coords[[i]],
        col    = vertex.color[i],
        border = vertex.color[i],
        lwd=1
      )

      # Border
      graphics::polygon(
        vertex.frame.coords[[i]],
        col    = vertex.frame.color[i],
        border = vertex.frame.color[i],
        lwd=1
      )


    }

  invisible({
    list(
      vertex.coords       = vertex.coords,
      vertex.color        = vertex.color,
      vertex.frame.coords = vertex.frame.coords,
      vertex.frame.color  = vertex.frame.color,
      edges.color         = edges.color,
      edges.coords        = edges.coords,
      edges.arrow.coords  = edges.arrow.coords,
      edges.width         = edge.width,
      xlim                = xlim,
      ylim                = ylim
    )
  })


}

