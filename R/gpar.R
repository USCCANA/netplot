
#' Creates a nested list of parameters to be passed to [set_gpar]
#' @noRd
parse_gpar <- function(...) {

  dots <- list(...)
  ans <- new.env(hash = FALSE)

  for (p in names(dots)) {

    # What is hidding there
    par <- strsplit(p, "\\.")[[1]]

    # Setting it up

    # If only length 2, then it sets global attributes for the component
    if (length(par) == 2) {

      # Is it a parameter?
      if (!(par[[2]] %in% .grid_par_names))
        next

      nam <- par[1]
      ans[[nam]] <- c(
        ans[[nam]],
        structure(
          list(dots[[p]], par[1]),
          names = c(par[2], "type")
          )
        )

    } else if (length(par) == 3) {

      # Is it a parameter?
      if (!(par[[3]] %in% .grid_par_names))
        next

      nam <- paste(par[1:2], collapse="_")


      ans[[nam]] <- c(
        ans[[nam]],
        structure(dots[[p]], names = par[3]),
        if (!length(ans[[nam]]))
          structure(
            list(par[1], par[2]),
            names = c("type", "element")
            )
        else
          NULL
      )

    }


  }

  as.list(ans)

}

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
  np_validate$is_netplot(x)
  np_validate$type(type)
  np_validate$elements(element, type)

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
    unname(split(x$.edgelist[idx,], idx)) #lapply(idx, function(e) x$.edgelist[e, ])

  n <- ifelse(type == "edge", x$.M, x$.N)
  dots <- extend_parameter(x = x, n = n, ...)

  # Updating the
  for (i in seq_along(idx)) {

    # Fabricating the name
    iname <- netplot_name$make(idx[[i]])

    ele <- x$children$graph$children[[iname]]$children[[element]]

    if (!missing(element) & !is.null(ele)) {

      x$children$graph$children[[iname]]$children[[element]] <-
        grid::editGrob(
            grob = ele,
            gp = do.call(grid::gpar, dots[[i]])
          )

    } else {

      x$children$graph$children[[iname]] <- grid::editGrob(
        grob = x$children$graph$children[[iname]],
        gp = do.call(grid::gpar, dots[[i]])
      )

    }

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

  # dots <- list(...)
  #
  # dots$x       <- x
  # dots$type    <- "edge"
  # dots$element <- element
  # dots$idx     <- idx
  set_gpar(x = x, type = "edge", element = element, idx = idx, ...)

}

#' @rdname set_gpar
#' @export
set_vertex_gpar <- function(x, element, idx, ...) {

  if (missing(element))
    element <- c("core", "frame")

  if (missing(idx))
    idx <- seq_len(x$.N)

  # Step 0: Check whether col or fill was passed through ...
  dots <- list(...)

  # Step 1: Check if col/fill is a formula
  if(inherits(dots$col, "formula") || inherits(dots$fill, "formula")) {

    # Step 2: Extract RHS of formula using terms()
    if (inherits(dots$col, "formula")) {
      color_var <- attr(terms(dots$col), "term.labels")
    }
    if (inherits(dots$fill, "formula")) {
      fill_var <- attr(terms(dots$fill), "term.labels")
    }

    # Step 3: Call color_nodes() with graph and attribute
    if (inherits(dots$col, "formula")) {
      dots$col <- color_nodes(x$.graph, color_var)
    }
    if (inherits(dots$fill, "formula")) {
      dots$fill <- color_nodes(x$.graph, fill_var)
    }

    # Adding information for the legend
    x$.legend_vertex_col <- dots$col
    x$.legend_vertex_fill <- dots$fill

  }

  do.call(
    set_gpar,
    c(
      list(x = x, type = "vertex", element = element, idx = idx),
      dots
    )
  )

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
  np_validate$is_netplot(x)
  np_validate$type(type)
  np_validate$elements(element, type)

  # If no id provided
  if (missing(idx))
    idx <- seq_len(ifelse(type == "vertex", x$.N, x$.M))

  # Creating loop sequence
  idx <- if (type == "vertex")
    as.list(idx)
  else
    unname(split(x$.edgelist[idx,], idx))#lapply(idx, function(e) x$.edgelist[e, ])

  # Basic cheks
  dots <- c(...)
  np_validate$gpar(dots)

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

