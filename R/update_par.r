

check_dots_update_gpar <- function(x, dots) {

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
update_gpar <- function(x, type, element, idx, ...) {

  # If elements is more than one
  if (!missing(element) && length(element) > 1) {

    for (e in element)
      x <- do.call(
        update_gpar,
        c(list(x, type, e, idx), list(...))
        )

    return(x)

  }

  # Checking element types
  netplot_elements$validate(element, type)

  # If no specific ids are provided
  n <- ifelse(type == "vertex", x$.N, x$.M)

  # Creating loop sequence
  if (missing(idx))
    seq_len(n)

  idx <- if (type == "vertex")
    as.list(idx)
  else
    lapply(idx, function(e) x$.edgelist[e, ])


  # Converting
  dots <- lapply(list(...), matrix, nrow = length(idx))

  # Basic cheks
  check_dots_update_gpar(x, dots)

  # Updating the
  for (i in seq_along(idx))
    x$children$graph$children[[netplot_name$make(idx[[i]])]]$children[[element]] <-
      grid::editGrob(
        grob = x$children$graph$children[[netplot_name$make(idx[[i]])]]$children[[element]],
        gp   = do.call(grid::gpar, lapply(dots, "[", i=i, j=))
      )

  # Returning the grob
  x

}

#' @rdname update_gpar
#' @description `update_edge_gpar` and `update_vertex_gpar` are shorthands for
#' `update_gpar(type = "edge", ...)` and `update_gpar(type = "vertex", ...)`
#' respectively.
#'
#' @export
update_edge_gpar <- function(x, element, idx, ...) {

  if (missing(element))
    element <- c("line", "arrow")

  if (missing(idx))
    idx <- seq_len(x$.M)

  update_gpar(x, type = "edge", element = element, idx = idx, ...)

}

#' @rdname update_gpar
#' @export
update_vertex_gpar <- function(x, element, idx, ...) {

  if (missing(element))
    element <- c("core", "frame")

  if (missing(idx))
    idx <- seq_len(x$.N)

  update_gpar(x, type = "vertex", element = element, idx = idx, ...)

}

