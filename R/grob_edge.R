

#' Computes edges
#' @noRd
grob_edge <- function(netenv, e) {

  # Obtaining positions of ego and alter
  i       <- netenv$edgelist[e, 1]
  j       <- netenv$edgelist[e, 2]
  nbreaks <- netenv$edge.line.breaks[e]

  if (nbreaks >= 0L) {

    # Computing coordinates
    coords <- arc(
      p0    = netenv$layout[i,],
      p1    = netenv$layout[j,],
      radii = netenv$vertex.size[c(i,j)] + c(0, netenv$arrow.size.adj[e]),
      alpha = netenv$edge.curvature[e],
      n     = nbreaks
    )

    # Generating grob
    if (!netenv$skip.edges) {

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

    } else
      ans <- NULL

    # Arrow
    # Computing arrow
    if (!netenv$skip.arrows) {

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

    } else
      ans <- grid::grobTree(ans, name = netplot_name$make(c(i, j)))



  } else {

    stop("Line breaks should be positive.", call. = FALSE)

    # The lines that follow should be implemented in the future!
    # Right now these are not working

    # Generating grob
    ans <- grid::curveGrob(
      x1          = netenv$layout[i,1],
      y1          = netenv$layout[i,2],
      x2          = netenv$layout[j,1],
      y2          = netenv$layout[j,2],
      ncp         = 6L,
      curvature   = .5,
      default.units = "native",
      gp         = do.call(
        grid::gpar,
        list(
          lwd     = netenv$edge.width[e],
          lineend = "butt",
          lty     = netenv$edge.line.lty[e]
        )
      ), arrow = grid::arrow(),
      name = "line"
    )

    ans <- grid::grobTree(
      ans,
      name = netplot_name$make(c(i, j))
    )

  }

  ans

}
