

#' Set/retrieve graphical parameters of a `netplot` object
#'
#' @param x An object of class `netplot`.
#' @param type Character. Either `"edge"` or `"vertex"`.
#' @param element Character. If `"edge"`, then it can be either `"line"` or
#' `"arrow"`, otherwise it can be either `"core"` or `"frame"`.
#' @param idx (optional) Integer vector. Indices of the elements to be modified.
#' When missing, all elements are modified.
#' @param ... Parameters to be modified/retrieved. This is passed to [grid::editGrob]
#' via [grid::gpar].
#'
#' @return An object of class `netplot` with modified parameters.
#'
#' @examples
#' library(igraph)
#' library(netplot)
#'
#' x <- make_ring(5)
#'
#' g <- nplot(x)
#'
#' # Updating edge color
#' g <- set_edge_gpar(g, col = "gray80")
#'
#' # Retrieving the color of the vertices (core)
#' get_vertex_gpar(g, element = "core", "fill", "lwd")
#'
#' @export
set_gpar <- function(x, type, element, idx, ...) {

  # Validations
  netplot_validate$is_netplot(x)
  netplot_validate$type(type)
  netplot_validate$elements(element, type)

  # If elements is more than one
  if (!missing(element) && length(element) > 1) {

    for (e in element)
      x <- do.call(
        set_gpar,
        c(list(x, type, e, idx), list(...))
        )

    return(x)

  }

  # If no specific ids are provided
  if (missing(idx))
    idx <- seq_len(ifelse(type == "vertex", x$.N, x$.M))

  # Creating loop sequence
  idx <- if (type == "vertex")
    as.list(idx)
  else
    lapply(idx, function(e) x$.edgelist[e, ])

  # Converting
  dots <- lapply(list(...), matrix, nrow = length(idx))

  # Basic cheks
  netplot_validate$gpar(dots)

  # Updating the
  for (i in seq_along(idx)) {

    # Fabricating the name
    iname <- netplot_name$make(idx[[i]])

    x$children$graph$children[[iname]]$children[[element]] <- grid::editGrob(
        grob = x$children$graph$children[[iname]]$children[[element]],
        gp    = do.call(grid::gpar, lapply(dots, "[", i=i, j=))
      )
  }

  # Returning the grob
  x

}

#' @rdname set_gpar
#' @details
#' `set_edge_gpar` and `set_vertex_gpar` are shorthands for
#' `set_gpar(type = "edge", ...)` and `set_gpar(type = "vertex", ...)`
#' respectively.
#'
#' @export
set_edge_gpar <- function(x, element, idx, ...) {

  if (missing(element))
    element <- c("line", "arrow")

  if (missing(idx))
    idx <- seq_len(x$.M)

  set_gpar(x, type = "edge", element = element, idx = idx, ...)

}

#' @rdname set_gpar
#' @export
set_vertex_gpar <- function(x, element, idx, ...) {

  if (missing(element))
    element <- c("core", "frame")

  if (missing(idx))
    idx <- seq_len(x$.N)

  set_gpar(x, type = "vertex", element = element, idx = idx, ...)

}

#' @rdname set_gpar
#' @details
#' `get_edge_gpar` and `get_vertex_gpar` are shorthands for
#' `get_gpar(type = "edge", ...)` and `get_gpar(type = "vertex", ...)`
#' respectively.
#' @export
get_vertex_gpar <- function(x, element, ..., idx) {

  if (missing(idx))
    idx <- seq_len(x$.N)

  get_gpar(x = x, type = "vertex", element = element, ..., idx = idx)

}

#' @rdname set_gpar
#' @export
get_edge_gpar <- function(x, element, ..., idx) {

  if (missing(idx))
    idx <- seq_len(x$.M)

  get_gpar(x = x, type = "edge", element = element, ..., idx = idx)

}

#' @rdname set_gpar
#' @param simplify Logical. When `TRUE` it tries to simplify the result.
#' Otherwise it returns a nested list.
#' @export
get_gpar <- function(x, type, element, ..., idx, simplify=TRUE) {

  # Checking type and element
  netplot_validate$is_netplot(x)
  netplot_validate$type(type)
  netplot_validate$elements(element, type)

  # If no id provided
  if (missing(idx))
    idx <- seq_len(ifelse(type == "vertex", x$.N, x$.M))

  # Creating loop sequence
  idx <- if (type == "vertex")
    as.list(idx)
  else
    lapply(idx, function(e) x$.edgelist[e, ])

  # Basic cheks
  dots <- c(...)
  netplot_validate$gpar(dots)

  n   <- length(idx)
  ans <- lapply(seq_along(dots), function(i) vector("list", n))
  names(ans) <- dots

  for (p in dots)
    for (i in seq_along(idx)) {

      # Getting the component
      v <- x$children$graph$children[[netplot_name$make(idx[[i]])]]

      # If it exists
      if (!length(v) || !length(v$children[[element]])) {
        ans[[p]][[i]] <- NA
        next
       }

      # And it has the element
      v <- v$children[[element]]$gp[[p]]

      if (length(v))
        ans[[p]][[i]] <- v
      else
        ans[[p]][[i]] <- NA

    }

  if (simplify)
    ans <- lapply(ans, base::simplify2array)

  ans

}
