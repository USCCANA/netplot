

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

  # Computing colors using colorRamp2
  if (!length(netenv$edge.color) | (length(netenv$edge.color) && is.na(netenv$edge.color[e]))) {

    col <- polygons::colorRamp2(
      c(netenv$vertex.color[i], netenv$vertex.color[j])
    )(netenv$edge.color.mix[e])

    col <- rgb(col, maxColorValue = 255)

  } else {
    col <- netenv$edge.color[e]
  }

  col <- polygons::colorRamp2(
    c(
      adjustcolor(col, alpha.f = netenv$edge.color.alpha[e,1]),
      adjustcolor(col, alpha.f = netenv$edge.color.alpha[e,2])
    ), alpha = TRUE)

  col <- col(seq(0,1, length.out = nbreaks))
  col <- grDevices::rgb(col, alpha = col[,4], maxColorValue = 255)



  # Generating grob
  ans <- grid::polylineGrob(
    x          = coords[,1],
    y          = coords[,2],
    id.lengths = rep(2, nbreaks),
    default.units = "native",
    gp         = grid::gpar(
      col = netenv$edge.color,
      lty = netenv$edge.line.lty[e],
      lwd = netenv$edge.width[e],
      lineend = "butt"
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

  ans <- grid::grobTree(
    ans,
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
    name = paste0("edge.", i, "-",j)
  )

  ans

}
