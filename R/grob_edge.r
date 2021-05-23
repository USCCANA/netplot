

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

  # Generating grob
  ans <- grid::polylineGrob(
    x          = coords[,1],
    y          = coords[,2],
    id.lengths = rep(2, nbreaks),
    default.units = "native",
    gp         = do.call(
      grid::gpar,
      list(
        lwd     = netenv$edge.width[e],
        lineend = "butt",
        lty     = netenv$edge.line.lty[e]
        )
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
        lwd  = netenv$edge.width[e]
      )
    ),
    name = netplot_name$make(c(i, j))
  )

  ans

}
