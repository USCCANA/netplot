  sides_lookup <- list(
    line     = list(sides = 2, rot = 0),    # 1
    triangle = list(sides = 3, rot = 0),    # 2
    square   = list(sides = 4, rot = pi/2), # 3
    diamond  = list(sides = 4, rot = 0),    # 4
    pentagon = list(sides = 5, rot = 0),    # 5
    hexagon  = list(sides = 6, rot = 0),    # 6
    heptagon = list(sides = 7, rot = 0),    # 7
    octagon  = list(sides = 8, rot = 0),    # 8
    circle   = list(sides = 25, rot = 0)    # 9
  )

#' Functions to calculate graph polygons coordinates
#' @param netenv An object of class network environment.
#' @param v,e Integer scalars. vertex or edge index.
#'
#' @return A grob
#' Generating coordinates
#' @noRd
grob_vertex <- function(netenv, v) {

  # Add formula handling
  if(inherits(netenv$vertex.nsides, "formula")) {
    var_name <- all.vars(netenv$vertex.nsides)[1]
    netenv$vertex.nsides <- eval(netenv$vertex.nsides, envir = data)
  }

  # Relax vertex.nsides validation
  if(!is.numeric(netenv$vertex.nsides[v]) & !is.character(netenv$vertex.nsides[v])) {
    stop("vertex.nsides must be numeric or character")
  }

  # Handle shape names
  if(is.character(netenv$vertex.nsides[v])) {

    shape <- netenv$vertex.nsides[v]

    if(shape %in% names(sides_lookup)) {
      info <- sides_lookup[[shape]]
      netenv$vertex.nsides[v] <- info$sides
      netenv$vertex.rot[v] <- info$rot
    } else {
      stop("Invalid shape name: ", shape)
    }

  }



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
    n = as.integer(netenv$vertex.nsides[v]),
    r = netenv$vertex.size[v]*(1 - netenv$vertex.frame.prop[v]),
    d = as.integer(netenv$vertex.rot[v])
  )

  # Frame coordinates
  framecoords <- npolygon(
    x = netenv$layout[v, 1],
    y = netenv$layout[v, 2],
    n = as.integer(netenv$vertex.nsides[v]),
    r = netenv$vertex.size[v],
    d = as.integer(netenv$vertex.rot[v])
  )

  # Create color palette
  nsides <- unique(netenv$vertex.nsides)
  ncolors <- length(nsides)
  colors <- hsv(h = seq(0, 1, length.out = ncolors), v = 1, a = 1)
  pal <- setNames(colors, nsides)

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
