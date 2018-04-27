#' Rotation of polygon
#' @param mat Two-column numeric matrix. Coordinates of the polygon.
#' @param p0 Numeric vector of length two. Origin.
#' @param alpha Numeric scalar. Rotation degree in radians.
#' @export
rotate <- function(mat, p0, alpha) {
  R <- matrix(
    c(cos(alpha), -sin(alpha), sin(alpha), cos(alpha)),
    nrow = 2, byrow = TRUE)

  p0 <- matrix(c(p0[1], p0[2]), ncol=2, nrow = nrow(mat), byrow = TRUE)
  t(R %*% t(mat - p0)) + p0
}

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
  n     = 40L,
  radii = c(0, 0)
) {

  # If no curve, nothing to do (old fashioned straight line)
  if (alpha == 0) {
    alpha <- 1e-5
    n     <- 3
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
    length.out = n
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
  ans <- rotate(ans, p0, elevation)

  structure(
    ans,
    alpha0 = atan2(ans[1,2] - p0[2], ans[1,1] - p0[1]),
    alpha1 = atan2(p1[2] - ans[n,2], p1[1] - ans[n,1]),
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

  rotate(ans, p_top, alpha = alpha)


}

#' Rescale the size of a node to make it relative to the aspect ratio of the device
#' @param size Numeric vector. Size of the node (radious).
#' @param rel Numeric vector of length 3. Relative size for the minimum and maximum
#' of the plot, and curvature of the scale. The third number is used as `size^rel[3]`.
#'
#' @details
#' This function is to be called after [plot.new], as it takes the parameter `usr`
#' from the
rescale_node <- function(size, rel=c(.01, .05, 3)) {

  # Checking the rel size
  if (length(rel) == 2)
    rel <- c(rel, 3)
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
  usr <- graphics::par()$usr[1:2]
  size * (usr[2] - usr[1])/2

}

#' Function to rescale the edge-width.
#' @param rel Vector of length 2 with the min and max width.
#' @param width Numeric vector. width of the edges.
#' @export
rescale_edge <- function(width, rel=c(1, 3)) {

  ran   <- range(width, na.rm = TRUE)
  if (ran[1] != ran[2])
    width <- (width - ran[1])/(ran[2] - ran[1])
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
fit_coords_to_dev <- function(coords, adj = graphics::par("pin")[1:2]) {

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


#' A wrapper of `rgb(colorRamp)`
#' @param i,j Integer scalar. Indices of ego and alter from 1 through n.
#' @param p Numeric scalar from 0 to 1. Proportion of mixing.
#' @param vcols Vector of colors.
#' @param alpha Numeric scalar from 0 to 1. Passed to [grDevices::colorRamp()]
#' @return A color.
edge_color_mixer <- function(i, j, vcols, p = .5, alpha = .15) {

  grDevices::adjustcolor(grDevices::rgb(
    grDevices::colorRamp(vcols[c(i,j)], alpha = FALSE)(p),
    maxColorValue = 255
  ), alpha = alpha)

}


line_gradient <- function(x, y = NULL, col.alpha.range = c(0, 1), col = "black",...) {

  # Getting new coords
  x   <- grDevices::xy.coords(x, y)
  n   <- length(x$x) - 1L
  col <- sapply(0:(n-1)/(n-1), grDevices::adjustcolor, col=col)

  # Creating traces
  idx <- lapply(apply(cbind(1:n, 2:(n+1)), 1, list), unlist)
  y   <- lapply(idx, function(i) x$y[i])
  x   <- lapply(idx, function(i) x$x[i])

  invisible(Map(function(.x, .y, .col, ...) lines(x=.x, y = .y, col=.col, ...),
      .col=col, .x=x, .y=y, ...))

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
#' @param edge.color.alpha Numeric vector of length `ecount(x)` with values in
#' `[0,1]`. Alpha (transparency) levels.
#' @param edge.line.type Vector of length `ecount(x)`.
#' @param edge.line.lty Vector of length `ecount(x)`.
#' @param edge.line.breaks Vector of length `ecount(x)`. Number of vertices to
#' draw (approximate) the arc (edge).
#' @param edge.line.pch  Vector of length `ecount(x)`.
#' @export
#' @importFrom viridis viridis
#' @importFrom igraph layout_with_fr degree vcount ecount
#' @importFrom grDevices adjustcolor
#' @importFrom graphics lines par plot polygon rect
#' @importFrom polygons piechart npolygon
#' @examples
#' library(igraph)
#' library(netplot)
#' set.seed(1)
#' x <- sample_smallworld(1, 200, 5, 0.03)
#'
#' plot(x) # ala igraph
#' nplot(x) # ala netplot
nplot <- function(
  x,
  layout              = igraph::layout_with_fr(x),
  vertex.size         = igraph::degree(x),
  bg.col              = "lightgray",
  vertex.shape        = 100,
  vertex.color        = NULL,
  vertex.size.range   = c(.01, .03),
  vertex.frame.color  = NULL,
  vertex.shape.degree = 0,
  edge.width          = NULL,
  edge.width.range    = c(1, 2),
  edge.arrow.size     = NULL,
  edge.color.mix      = .5,
  edge.color.alpha    = .5,
  edge.curvature      = pi/3,
  edge.line.type      = NULL,
  edge.line.lty       = "solid",
  edge.line.breaks    = 20,
  edge.line.pch       = 20
) {

  # Computing colors
  if (!length(vertex.color)) {
    vertex.color <- length(table(igraph::degree(x)))
    vertex.color <- viridis::viridis(vertex.color)
    vertex.color <- vertex.color[
      as.factor(igraph::degree(x))
      ]
  }

  # # Creating the window
  oldpar <- graphics::par(mai=rep(.05, 4))
  on.exit(graphics::par(oldpar))

  # Adjusting layout to fit the device
  layout <- fit_coords_to_dev(layout)

  # Plotting
  plot(layout, type = "n", bty="n", xaxt="n", yaxt="n", ylab="", xlab="", asp=1)

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


  # Weights
  if (!length(edge.width))
    edge.width <- rep(1.0, length(igraph::E(x)))

  # Rescaling edges
  edge.width <- rescale_edge(edge.width/max(edge.width, na.rm=TRUE), rel = edge.width.range)

  # Computing shapes -----------------------------------------------------------
  E <- igraph::as_edgelist(x, names = FALSE)

  if (!length(edge.arrow.size))
    edge.arrow.size <- vertex.size[E[,1]]/1.5

  # Calculating arrow adjustment
  arrow.size.adj <- edge.arrow.size*cos(pi/6)/(
    cos(pi/6) + cos(pi - pi/6 - pi/1.5)
  )/cos(pi/6)

  ans <- vector("list", nrow(E))

  if (length(edge.curvature) == 1)
    edge.curvature <- rep(edge.curvature, length(ans))

  if (length(edge.line.breaks) == 1)
    edge.line.breaks <- rep(edge.line.breaks, length(ans))

  for (e in 1:nrow(E)) {

    i <- E[e,1]
    j <- E[e,2]

    ans[[e]] <- arc(
      layout[i,], layout[j,],
      radii = vertex.size[c(i,j)] + c(0, arrow.size.adj[e]),
      alpha = edge.curvature[e],
      n     = edge.line.breaks[e]
    )

  }

  # Edges
  if (!length(edge.color.mix))
    edge.color.mix <- rep(.5, length(ans))
  else if (length(edge.color.mix) == 1)
    edge.color.mix <- rep(edge.color.mix, length(ans))

  if (!length(edge.line.type))
    edge.line.type <- rep("l", length(ans))
  else if (length(edge.line.type) == 1)
    edge.line.type <- rep(edge.line.type, length(ans))

  if (!length(edge.line.lty))
    edge.line.lty <- rep(1L, length(ans))
  else if (length(edge.line.lty) == 1)
    edge.line.lty <- rep(edge.line.lty, length(ans))

  if (!length(edge.line.pch))
    edge.line.pch <- rep(20L, length(ans))
  else if (length(edge.line.pch) == 1)
    edge.line.pch <- rep(edge.line.pch, length(ans))

  if (!length(edge.color.alpha))
    edge.color.alpha <- rep(.5, length(ans))
  else if (length(edge.color.alpha) == 1)
    edge.color.alpha <- rep(edge.color.alpha, length(ans))

  # if (!length(edge.color.alpha))
  #   edge.color.alpha <- rep(.5, igraph::ecount(x))
  # else if (length(edge.color.alpha) == 1)
  #   edge.color.alpha <- rep(edge.color.alpha, igraph::ecount(x))


  for (i in seq_along(ans)) {

    if (!length(ans[[i]]))
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
      alpha = edge.color.alpha[i]
      )

    # Drawing lines
    lines(ans[[i]], lwd= edge.width[i], col = col, type = edge.line.type[i],
          lty = edge.line.lty[i], pch = edge.line.pch[i])

    # Computing arrow
    alpha1 <- attr(ans[[i]], "alpha1")
    arr <- arrow_fancy(
      x = ans[[i]][nrow(ans[[i]]),1:2] +
        arrow.size.adj[i]*c(cos(alpha1), sin(alpha1)),
      alpha = alpha1,
      l     = edge.arrow.size[i]
    )

    # Drawing arrows
    graphics::polygon(
      arr,
      col    = col,
      border = col,
      lwd    = edge.width[i]
    )

  }

  if (!length(vertex.frame.color))
    vertex.frame.color <- adjustcolor(vertex.color, red.f = .75, blue.f = .75, green.f = .75)

  # Nodes
  if (length(vertex.shape) == 1)
    vertex.shape <- rep(vertex.shape, nrow(layout))

  if (length(vertex.shape.degree) == 1)
    vertex.shape.degree <- rep(vertex.shape.degree, nrow(layout))

  for (i in 1:nrow(layout)) {

    # Circle
    graphics::polygon(
      polygons::npolygon(
        layout[i,1], layout[i,2],
        n = vertex.shape[i],
        r = vertex.size[i]*.9,
        vertex.shape.degree[i]
      ),
      col    = vertex.color[i],
      border = vertex.color[i],
      lwd=1
    )

    # Border
    graphics::polygon(
      polygons::piechart(
        1,
        origin = layout[i,],
        edges  = vertex.shape[i],
        radius = vertex.size[i],
        doughnut = vertex.size[i]*.9,
        rescale = FALSE,
        add     = TRUE,
        skip.plot.slices = TRUE
      )$slices[[1]],
      col    = vertex.frame.color[i],
      border = vertex.frame.color[i],
      lwd=1
    )


  }

}

