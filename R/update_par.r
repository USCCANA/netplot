

check_dots_update_par <- function(x, dots) {

  # Only works for netplot
  stopifnot(inherits(x, "netplot"))

  # Are the par aesthetics registered?
  test <- which(!(names(dots) %in% names(grid::get.gpar())))
  if (length(test))
    stop("The following parameters are not part of `grid::gpar`: '",
         paste(names(dots[test]), collapse="', '"), "'.",
         "Supported parameters are: '",
         paste(names(grid::get.gpar()), collapse="', '"),"'.",
         call. = FALSE)


  invisible()
}

#' Update graphical parameters of a `netplot` object
#'
#' @param x An object of class `netplot`.
#' @param type Character. Either `"edge"` or `"vertex"`.
#' @param element Character. If `"edge"`, then it can be either `"line"` or
#' `"arrow"`, otherwise it can be either `"core"` or `"frame"`.
#' @param idx (optional) Integer vector. Indices of the elements to be modified.
#' When missing, all elements are modified.
#' @param ... Parameters to be modified. This is passed to [grid::editGrob]. The
#' complete list of parameters that can be modified is available in [grid::gpar].
#'
#' @return An object of class `netplot` with modified parameters.
#' @export
update_par <- function(x, type, element, idx, ...) {

  # If no specific ids are provided
  if (type == "vertex") {

    n    <- x$.N
    type <- function(i) paste0("vertex.", idx[i])

    if (!(element %in% c("core", "frame", "label")))
      stop("Invalid type of `element`.", call. = FALSE)

  } else if (type == "edge") {

    n    <- x$.N
    type <- function(i) sprintf(
      "edge.%i-%i",
      x$.edgelist[idx[i], ][1],
      x$.edgelist[idx[i], ][2]
      )

    if (!(element %in% c("arrow", "line")))
      stop("Invalid type of `element`.", call. = FALSE)

  } else
    stop("The `type` should be either 'vertex' or 'edge'.", call. = FALSE)


  if (missing(idx))
    idx <- seq_len(n)

  # Converting
  dots <- lapply(list(...), matrix, nrow = length(idx))

  # Basic cheks
  check_dots_update_par(x, dots)

  # Updating the
  for (i in seq_along(idx))
    x$children$graph$children[[type(i)]]$children[[element]] <-
      grid::editGrob(
        grob = x$children$graph$children[[type(i)]]$children[[element]],
        gp   = do.call(grid::gpar, lapply(dots, "[", i=i, j=))
      )

  # Returning the grob
  x

}

#' @rdname update_par
#' @description `update_edge_par` and `update_vertex_par` are shorthands for
#' `update_par(type = "edge", ...)` and `update_par(type = "vertex", ...)`
#' respectively.
#'
#' @export
update_edge_par <- function(x, element, idx, ...) {

  update_par(x, type = "edge", element = element, idx = idx, ...)

}

#' @rdname update_par
#' @export
update_vertex_par <- function(x, element, idx, ...) {

  update_par(x, type = "vertex", element = element, idx = idx, ...)

}

