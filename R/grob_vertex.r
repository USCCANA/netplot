#' Functions to calculate graph polygons coordinates
#' @param netenv An object of class network environment.
#' @param v,e Integer scalars. vertex or edge index.
#'
#' @return A grob
#' Generating coordinates
#' @noRd
grob_vertex <- function(netenv, v) {

  # Computing coordinates
  coords <- npolygon(
    x = netenv$layout[v, 1],
    y = netenv$layout[v, 2],
    n = netenv$vertex.nsides[v],
    r = netenv$vertex.size[v]*(1 - netenv$vertex.frame.prop[v]),
    d = netenv$vertex.rot[v]
  )

  # Frame coordinates
  framecoords <- npolygon(
    x = netenv$layout[v, 1],
    y = netenv$layout[v, 2],
    n = netenv$vertex.nsides[v],
    r = netenv$vertex.size[v],
    d = netenv$vertex.rot[v]
  )

  # Returning
  ans <- grid::polygonGrob(
    x    = c(framecoords[,1], coords[,1]),
    y    = c(framecoords[,2], coords[,2]),
    id.lengths = c(nrow(coords), nrow(framecoords)),
    gp   = grid::gpar(
      fill = c(netenv$vertex.frame.color[v],netenv$vertex.color[v]),
      col  = c(netenv$vertex.frame.color[v],netenv$vertex.color[v])
    ),
    default.units = "native",
    name = paste0("vertex.", v)
  )

  # If the users is drawing text
  if (length(netenv$vertex.label) && !is.na(netenv$vertex.label[v])) {

    # Only if it is big enough
    if (netenv$label_threshold <= netenv$vertex.size[v])
      ans <- grid::gList(
        ans,
        grid::textGrob(
          label = netenv$vertex.label[v],
          x     = netenv$layout[v, 1],
          y     = netenv$layout[v, 2] + netenv$vertex.size[v]*1.1,
          gp    = grid::gpar(
            fontsize   = netenv$vertex.label.fontsize[v],
            col        = netenv$vertex.label.color[v],
            fontfamily = netenv$vertex.label.fontfamily[v],
            fontface   = netenv$vertex.label.fontface[v]
          ),
          vjust         = 0,
          default.units = "native",
          name = paste0("vertex-label.", v)
        )
      )

  }

  ans
}
