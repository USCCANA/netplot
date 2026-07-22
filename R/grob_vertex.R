  sides_lookup <- list(
    line     = list(sides = 2, rot = 0),    # 1
    triangle = list(sides = 3, rot = 0),    # 2
    square   = list(sides = 4, rot = pi/4), # 3
    diamond  = list(sides = 4, rot = 0),    # 4
    pentagon = list(sides = 5, rot = 0),    # 5
    hexagon  = list(sides = 6, rot = 0),    # 6
    heptagon = list(sides = 7, rot = 0),    # 7
    octagon  = list(sides = 8, rot = 0),    # 8
    circle   = list(sides = 25, rot = 0)    # 9
  )

resolve_vertex_shape <- function(nsides, rot) {

  if (!is.numeric(nsides) & !is.character(nsides)) {
    stop("vertex.nsides must be numeric or character")
  }

  rot <- as.numeric(rot)

  if (is.character(nsides)) {

    shape <- nsides

    if (shape %in% names(sides_lookup)) {
      info <- sides_lookup[[shape]]
      return(list(sides = info$sides, rot = info$rot + rot))
    } else {
      stop("Invalid shape name: ", shape)
    }

  }

  list(sides = nsides, rot = rot)

}

#' Functions to calculate graph polygons coordinates
#' @param netenv An object of class network environment.
#' @param v,e Integer scalars. vertex or edge index.
#'
#' @return A grob
#' Generating coordinates
#' @noRd
grob_vertex <- function(netenv, v) {

  # # Add formula handling
  # if(inherits(netenv$vertex.nsides, "formula")) {
  #   var_name <- all.vars(netenv$vertex.nsides)[1]
  #   netenv$vertex.nsides <- eval(netenv$vertex.nsides, envir = data)
  # }

  vertex_shape <- resolve_vertex_shape(
    nsides = netenv$vertex.nsides[v],
    rot    = netenv$vertex.rot[v]
  )

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
    n = as.integer(vertex_shape$sides),
    r = netenv$vertex.size[v]*(1 - netenv$vertex.frame.prop[v]),
    d = vertex_shape$rot
  )

  # Frame coordinates
  framecoords <- npolygon(
    x = netenv$layout[v, 1],
    y = netenv$layout[v, 2],
    n = as.integer(vertex_shape$sides),
    r = netenv$vertex.size[v],
    d = vertex_shape$rot
  )

  # Create color palette
  nsides <- unique(netenv$vertex.nsides)
  ncolors <- length(nsides)
  colors <- grDevices::hsv(h = seq(0, 1, length.out = ncolors), v = 1, a = 1)
  pal <- stats::setNames(colors, nsides)

  # Lookup color for this vertex based on number of sides
  col <- pal[as.character(netenv$vertex.nsides[v])]

  # Returning
  ans <- grid::polygonGrob(
    x = c(coords[,1]),
    y = c(coords[,2]),
    gp = grid::gpar(
      fill = col,
      col = col
    ),
    default.units = "native",
    name = "core"
  )

  ans <- grid::grobTree(
    grid::polygonGrob(
      x = framecoords[,1],
      y = framecoords[,2],
      gp = grid::gpar(
        fill = col,
        col = col
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
    if (netenv$label_threshold <= netenv$vertex.size[v]) {

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
          name          = "label",
          check.overlap = TRUE
        )
      )

    }
  }
  ans
}
