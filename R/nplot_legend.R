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
#'   vertex.color  = ifelse(V(x)$nsides == 4, "red", "steelblue")
#'   )
#'
#' nplot_legend(
#'   g,
#'   labels = c("circle", "diamond"),
#'   pch    = c(21, 23),
#'   gp     = gpar(fill = c("steelblue", "red"))
#'   )
nplot_legend <- function(
    g,
    labels,
    pch,
    gp = gpar(),
    ...,
    packgrob.args = list(side = "left")
    ) {

  # Cleaning up
  if (newpage)
    grid::grid.newpage()

  # Creating the new frame
  gf <- grid::packGrob(grid::frameGrob(), g)

  # Adding the legend
  legend.args   <- do.call(grid::legendGrob, c(list(labels = labels, pch = pch, gp = gp), list(...)))
  packgrob.args <- c(list(frame = gf, grob = legend.args), packgrob.args)

  gf <- do.call(grid::packGrob, packgrob.args)

  # Setting the right names
  gf$children[[1]] <- editGrob(gf$children[[1]], name = grobName(gf$children[[1]], "graph"))
  gf$children[[2]] <- editGrob(gf$children[[2]], name = grobName(gf$children[[2]], "legend"))

  structure(
    gf,
    class = "netplot_legend"
  )

}

#' @export
#' @rdname nplot_legend
print.netplot_legend <- function(x, y = NULL, newpage = TRUE, ...) {

  if (newpage) {
    grid::grid.newpage()
  }

  grid::grid.draw(x)

  invisible(x)

}
