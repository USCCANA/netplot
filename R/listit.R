
# dots <- radialGradient()
#
# n     <- 100
# n_par <- length(dots)
#
# for (e in list(list(), vector("integer", 2), 1L, "p")) {
#   cat(sprintf(
#     "Mode: %9s; is.atomic: %5s; is.vector: %5s; typeof(): %-9s = %s\n",
#     mode(e),
#     is.atomic(e),
#     is.vector(e),
#     typeof(e),
#     deparse(e)
#   ))
# }


#' Extends the size of the parameters
#'
#' Atomic types (integers, characters, etc.) are extended automatically. In
#' the case of patterns (from the grid package,) if first checks the type. If
#' the ... is a list, then the only valids are size 1 or n.
#'
#' @param x An object of class netplot.
#' @param n Length to recycle the parameters.
#' @param ... Parameters from gpar.
#' @noRd
extend_parameter <- function(x, n, ...) {

  dots <- list(...)
  np_validate$gpar(dots)

  for (i in names(dots)) {

    n_i <- length(dots[[i]])

    # Case 1: Is logical/scalar/int/char ---------------------------------------
    if (is.atomic(dots[[i]])) {

      if (n_i == 1) {

        dots[[i]] <- rep(dots[[i]], times = n)

      } else if (n_i != n) {

        stop(
          sprintf(
            "The value passed to %s: %s, is not valid.",
            i, deparse(dots[[i]])
            )
          )

      }

    }
    # Case 2: Formula ----------------------------------------------------------
    else if (inherits(dots[[i]], "formula")) {

      dots[[i]] <- netplot_edge_formulae(x, dots[[i]])

    # Case 3: A list -----------------------------------------------------------
    } else {

      # Case 3a: A pattern list
      if (inherits(dots[[i]], c("GridRadialGradient", "GridPattern"))) {

        dots[[i]] <- replicate(n, list(dots[[i]]), simplify = FALSE)
        next

      }

      # Case 3a: A list of colors
      if (n_i == n) {
        next
      } else {
        stop("The length of ", i, " is invalid.")
      }

    }

  }

  # Returning
  do.call(Map, c(dots, list(f = function(...) {
    structure(list(...), names = names(list(...)))
  })))

}
