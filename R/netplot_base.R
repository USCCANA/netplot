#' `nplot` using base graphics
#' @param edge.color.mix Proportion of the mixing.
#' @param edge.color.alpha Either a vector of length 1 or 2, or a matrix of
#' size `ecount(x)*2` with values in `[0,1]`. Alpha (transparency) levels (see
#' details)
#' @inheritParams nplot
#' @export
#' @return `nplot_base` returns a list with the following components:
#'
#' - `vertex.coords` A list of length `N` where each element describes the
#' geomtry of each vertex.
#' - `vertex.color` A vector of colors
#' - `vertex.frame.coords` Similar to `vertex.coords`, but for the frame.
#' - `vertex.frame.color` Similar to `vertex.color`, but for the frame.
#' - `edge.color` Vector of functions used to compute the edge colors.
#' - `edge.coords` Similar to `vertex.coords`, the points that describe each
#' edge.
#' - `edge.arrow.coords` A list of matrices describing the geometry of the
#' tip of the edges.
#' - `edge.width` A numeric vector with edges' widths.
#' - `xlim`, `ylim` Limits of the plot area.
#' @seealso [nplot]
#' @examples
#' # Same example as in nplot
#' library(igraph)
#' library(netplot)
#' set.seed(1)
#' x <- sample_smallworld(1, 200, 5, 0.03)
#'
#' nplot_base(x) # ala netplot (using base)
nplot_base <- function(
  x,
  layout              = igraph::layout_nicely(x),
  vertex.size         = igraph::degree(x, mode="in"),
  bg.col              = "transparent",
  vertex.nsides       = 10,
  vertex.color        = grDevices::hcl.colors(1),
  vertex.size.range   = c(.01, .03, 4),
  vertex.frame.color  = grDevices::adjustcolor(vertex.color, red.f = .75, green.f = .75, blue.f = .75),
  vertex.rot          = 0,
  vertex.frame.prop   = .1,
  edge.width          = NULL,
  edge.width.range    = c(1, 2),
  edge.arrow.size     = NULL,
  edge.color          = NULL,
  edge.color.mix      = .5,
  edge.color.alpha    = c(.1, .5),
  edge.curvature      = pi/3,
  edge.line.lty       = "solid",
  edge.line.breaks    = 5,
  sample.edges        = 1,
  skip.vertex         = FALSE,
  skip.edges          = FALSE,
  skip.arrows         = skip.edges,
  add                 = FALSE,
  zero.margins        = TRUE
) {

  # Checking layout
  if (!inherits(layout, what = c("matrix")))
    stop(
      "-layout- should be of class 'matrix'. It currently is '",
      class(layout),
      "'"
    )

  # Computing colors
  if (!length(vertex.color)) {
    vertex.color <- length(table(igraph::degree(x, mode = "in")))
    vertex.color <- grDevices::hcl.colors(vertex.color)
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
  vertex.size <- rescale_size(vertex.size, rel = vertex.size.range)

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
  edge.width <- rescale_size(edge.width/max(edge.width, na.rm=TRUE), rel = edge.width.range)

  if (!length(edge.arrow.size))
    edge.arrow.size <- vertex.size[E[,1]]/1.5
  else if (length(edge.arrow.size) == 1L)
    edge.arrow.size <- rep(edge.arrow.size, nrow(E))

  # Calculating arrow adjustment
  arrow.size.adj <- edge.arrow.size*cos(pi/6)/(
    cos(pi/6) + cos(pi - pi/6 - pi/1.5)
  )/cos(pi/6)

  # Making space for the arrow and edges coordinates
  edge.coords <- vector("list", nrow(E))
  edge.arrow.coords <- vector("list", length(edge.coords))

  if (length(edge.curvature) == 1)
    edge.curvature <- rep(edge.curvature, length(edge.coords))

  if (length(edge.line.breaks) == 1)
    edge.line.breaks <- rep(edge.line.breaks, length(edge.coords))


  for (e in 1:nrow(E)) {

    i <- E[e,1]
    j <- E[e,2]

    # Calculating edges coordinates
    edge.coords[[e]] <- arc(
      layout[i,], layout[j,],
      radii = vertex.size[c(i,j)] + c(0, arrow.size.adj[e]),
      alpha = edge.curvature[e],
      n     = edge.line.breaks[e]
    )

    # Computing arrow
    alpha1 <- attr(edge.coords[[e]], "alpha1")
    edge.arrow.coords[[e]] <- arrow_fancy(
      x = edge.coords[[e]][nrow(edge.coords[[e]]),1:2] +
        arrow.size.adj[e]*c(cos(alpha1), sin(alpha1)),
      alpha = alpha1,
      l     = edge.arrow.size[e]
    )

  }

  # Edges ----------------------------------------------------------------------
  if (!length(edge.color.mix))
    edge.color.mix <- rep(.5, length(edge.coords))
  else if (length(edge.color.mix) == 1)
    edge.color.mix <- rep(edge.color.mix, length(edge.coords))

  if (!length(edge.line.lty))
    edge.line.lty <- rep(1L, length(edge.coords))
  else if (length(edge.line.lty) == 1)
    edge.line.lty <- rep(edge.line.lty, length(edge.coords))

  if (!length(edge.color.alpha))
    edge.color.alpha <- matrix(.5, nrow= length(edge.coords), ncol=2)
  else if (length(edge.color.alpha) <= 2)
    edge.color.alpha <- matrix(edge.color.alpha, nrow= length(edge.coords), ncol=2, byrow = TRUE)

  # Nodes ----------------------------------------------------------------------
  if (length(vertex.color) == 1)
    vertex.color <- rep(vertex.color, nrow(layout))

  if (!length(vertex.frame.color))
    vertex.frame.color <- adjustcolor(vertex.color, red.f = .75, blue.f = .75, green.f = .75)

  if (length(vertex.nsides) == 1)
    vertex.nsides <- rep(vertex.nsides, nrow(layout))

  if (length(vertex.rot) == 1)
    vertex.rot <- rep(vertex.rot, nrow(layout))

  if (length(vertex.frame.prop) == 1)
    vertex.frame.prop <- rep(vertex.frame.prop, nrow(layout))

  edge.color <- vector("list", length(edge.coords))

  for (i in seq_along(edge.coords)) {

    if (!length(edge.coords[[i]]))
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

    edge.color[[i]] <- colorRamp2(
      c(
        adjustcolor(col, alpha.f = edge.color.alpha[i,1]),
        adjustcolor(col, alpha.f = edge.color.alpha[i,2])
      ), alpha = TRUE)

    # Drawing lines
    if (!skip.edges) {
      segments_gradient(
        edge.coords[[i]], lwd= edge.width[i],
        col = edge.color[[i]],
        lty = edge.line.lty[i]
      )
    }

    if (!skip.arrows) {
      # Drawing arrows
      graphics::polygon(
        edge.arrow.coords[[i]],
        col    = col,
        border = col,
        lwd    = edge.width[i]
      )
    }

  }

  vertex.frame.prop <- 1 - vertex.frame.prop

  vertex.coords <- vector("list", nrow(layout))
  vertex.frame.coords <- vector("list", nrow(layout))

  if (!skip.vertex)
    for (i in 1:nrow(layout)) {
      # Computing coordinates
      vertex.coords[[i]] <- npolygon(
        layout[i,1], layout[i,2],
        n = vertex.nsides[i],
        r = vertex.size[i]*vertex.frame.prop[i],
        vertex.rot[i]
      )
      vertex.frame.coords[[i]] <- piechart(
        1,
        origin = layout[i,],
        edges  = vertex.nsides[i],
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
      edge.color         = edge.color,
      edge.coords        = edge.coords,
      edge.arrow.coords  = edge.arrow.coords,
      edge.width         = edge.width,
      xlim                = xlim,
      ylim                = ylim
    )
  })


}
