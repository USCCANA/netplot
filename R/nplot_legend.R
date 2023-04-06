#' Add legend to a netplot object
#'
#' Legends in [grid] graphics is a bit more complicated than in base graphics.
#' The function `nplot_legend` is a wrapper of [grid::legendGrob()] that makes
#' the process easier. Besides `labels`, the main visual arguments for the
#' figure ar passed through the `gp` argument (see examples).
#'
#' @param g An object of class [netplot].
#' @param labels Character vector of labels.
#' @param pch See [graphics::points()].
#' @param gp An object of class [grid::gpar()]
#' @param ... Further arguments passed to [grid::legendGrob()].
#' @param packgrob.args List of arguments passed to [grid::packGrob()].
#' @param newpage Logical scalar. When `TRUE` it calls [grid::grid.newpage()].
#' @return A frame grob.
#' @export
#' @examples
#' library(igraph)
#' library(netplot)
#' set.seed(1)
#' x <- sample_smallworld(1, 200, 5, 0.03)
#' V(x)$nsides <- sample(c(10, 4), 200, replace = TRUE)
#'
#' g <- nplot(
#'   x,
#'   vertex.nsides = V(x)$nsides,
#'   vertex.color  = ifelse(V(x)$nsides == 4, "red", "steelblue"),
#'   edge.line.breaks = 5
#'   )
#'
#' nplot_legend(
#'   g,
#'   labels = c("circle", "diamond", "edge"),
#'   pch    = c(21, 23, NA),
#'   gp     = gpar(
#'     fill = c("steelblue", "red", NA),
#'     lwd  = c(NA, NA, 1),
#'     col  = c(NA, NA, "purple")
#'     )
#'   )
#' grid.text("Legend to the left (default)", y = unit(.95, "npc"), just = "bottom")
#'
#' nplot_legend(
#'   g,
#'   labels = c("circle", "diamond", "edge"),
#'   pch    = c(21, 23, NA),
#'   gp     = gpar(
#'     fill = c("steelblue", "red", NA),
#'     lwd  = c(NA, NA, 1),
#'     col  = c(NA, NA, "purple")
#'     ),
#'   # These two extra options set the legend to the bottom
#'   packgrob.args = list(side = "bottom"),
#'   ncol = 3
#'   )
#' grid.text("Legend bottom", y = unit(.95, "npc"), just = "bottom")
#'
#'
nplot_legend <- function(
    g,
    labels,
    pch,
    gp = grid::gpar(),
    ...,
    packgrob.args = list(side = "left")
    ) {

  # Creating the new frame
  gf <- grid::packGrob(grid::frameGrob(), g)

  # Adding the legend
  legend.args   <- do.call(
    grid::legendGrob,
    c(list(labels = labels, pch = pch, gp = gp), list(...))
    )

  packgrob.args <- c(list(frame = gf, grob = legend.args), packgrob.args)

  gf <- do.call(grid::packGrob, packgrob.args)

  # Setting the right names
  gf$children[[1]] <- editGrob(
    gf$children[[1]],
    name = grid::grobName(gf$children[[1]], "graph")
    )

  gf$children[[2]] <- editGrob(
    gf$children[[2]],
    name = grid::grobName(gf$children[[2]], "legend")
  )

  structure(
    gf,
    class = c("netplot_legend", class(gf))
  )

}

#' @export
#' @param x An object of class `netplot_legend`.
#' @param y Ignored.
#' @rdname nplot_legend
print.netplot_legend <- function(x, y = NULL, newpage = TRUE, ...) {

  if (newpage) {
    grid::grid.newpage()
  }

  grid::grid.draw(x)

  invisible(x)

}
