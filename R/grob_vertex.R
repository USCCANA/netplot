#' Functions to calculate graph polygons coordinates
#' @param netenv An object of class network environment.
#' @param v,e Integer scalars. vertex or edge index.
#'
#' @return A grob
#' Generating coordinates
#' @noRd
grob_vertex <- function(netenv, v) {

  if (netenv$skip.vertex)
    return(
      grid::gTree(
        children = grid::gList(
          grid::grob(name = "frame"),
          grid::grob(name = "core"),
          grid::grob(name = "label")
        ),
        name = netplot_name$make(v)
      )
    )

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
    x    = c(coords[,1]),
    y    = c(coords[,2]),
    gp   = grid::gpar(
      fill = netenv$vertex.color[v],
      col  = netenv$vertex.color[v]
    ),
    default.units = "native",
    name = "core"
  )

  ans <- grid::grobTree(
    grid::polygonGrob(
      x    = framecoords[,1],
      y    = framecoords[,2],
      gp   = grid::gpar(
        fill = netenv$vertex.frame.color[v],
        col  = netenv$vertex.frame.color[v]
      ),
      default.units = "native",
      name = "frame"
    ),
    ans,
    name = netplot_name$make(v)
  )

  # If the users is drawing text
  if (length(netenv$vertex.label) && !is.na(netenv$vertex.label[v])) {

    # Only if it is big enough
    if (netenv$label_threshold <= netenv$vertex.size[v])
      ans <- grid::addGrob(
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
          name = "label"
        )
      )

  }

  ans
}
