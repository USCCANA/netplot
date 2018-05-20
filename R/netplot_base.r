#' `nplot` using base graphics
#' @inheritParams nplot
#' @export
nplot_base <- function(
  x,
  layout              = igraph::layout_nicely(x),
  vertex.size         = igraph::degree(x, mode="in"),
  bg.col              = "lightgray",
  vertex.nsides       = 50,
  vertex.color        = NULL,
  vertex.size.range   = c(.01, .03),
  vertex.frame.color  = NULL,
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
  edge.line.breaks    = 15,
  sample.edges        = 1,
  skip.vertex       = FALSE,
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

  # Edges ----------------------------------------------------------------------
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

  if (!skip.vertex)
    for (i in 1:nrow(layout)) {
      # Computing coordinates
      vertex.coords[[i]] <- polygons::npolygon(
        layout[i,1], layout[i,2],
        n = vertex.nsides[i],
        r = vertex.size[i]*vertex.frame.prop[i],
        vertex.rot[i]
      )
      vertex.frame.coords[[i]] <- polygons::piechart(
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
      edges.color         = edges.color,
      edges.coords        = edges.coords,
      edges.arrow.coords  = edges.arrow.coords,
      edges.width         = edge.width,
      xlim                = xlim,
      ylim                = ylim
    )
  })


}
