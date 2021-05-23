#' Generate functions to create colors for edges
#' @noRd
new_edge_coloring <- function(
  vertex_color_i = NULL,
  vertex_alpha_i = NULL,
  vertex_color_j = NULL,
  vertex_alpha_j = NULL,
  edge_color = NULL,
  edge_alpha = NULL,
  mix = NULL
  ) {

  list(
    vertex = function(e, i, j, n) {

      # Adjusting alpha levels
      col_i <- vertex_color_i[i]
      col_j <- vertex_color_j[j]

      # Getting alpha levels
      alpha_i <- vertex_alpha_i[i]
      alpha_j <- vertex_alpha_j[j]

      # [OPTIONAL] ---------------------------------------------------------------
      # Linear combination between i and j
      col_i <- colorRamp2(x=c(col_i, col_j))(mix[e])
      col_i <- grDevices::rgb(col_i, alpha=col_i[,4], maxColorValue = 255)
      col_j <- col_i
      # --------------------------------------------------------------------------

      # Applying alpha levels and getting mix
      col <- colorRamp2(
        x = c(
          grDevices::adjustcolor(col = col_i, alpha.f = alpha_i),
          grDevices::adjustcolor(col = col_j, alpha.f = alpha_j)
        )
      )(seq(0, 1, length.out = n))

      # Returning
      grDevices::rgb(col, alpha = col[,4], maxColorValue = 255)

    },
    edge = function(e, i, j, n) {

      # Getting alpha levels
      alpha_i <- vertex_alpha_i[i]
      alpha_j <- vertex_alpha_j[j]

      # Edge level params
      col_ij   <- grDevices::adjustcolor(col = edge_color[e], alpha.f = edge_alpha[e])

      # Applying alpha levels and getting mix
      col <- colorRamp2(x=
        c(
          grDevices::adjustcolor(col = col_ij, alpha.f = alpha_i),
          grDevices::adjustcolor(col = col_ij, alpha.f = alpha_j)
        )
      )(seq(0, 1, length.out = n))

      # Returning
      grDevices::rgb(col, alpha = col[,4], maxColorValue = 255)

    }
  )



}


#' @noRd
#' @importFrom stats terms
netplot_edge_formulae <- function(x, fm) {

  # Basic validation
  np_validate$is_netplot(x)

  if (!inherits(fm, "formula"))
    stop("Not a formula", call. = FALSE)

  tm  <- stats::terms(fm)
  mat <- attr(tm, "factors")
  nam <- unique(gsub("\\(.+", "", rownames(mat)))

  # 1. Checking no repeated
  if (length(nam) > nrow(mat))
    stop("One or more terms are repated.", call. = FALSE)

  # 2. edge only goes alone
  if (("edge" %in% nam) & length(nam) > 1)
    stop("`edge` must be used alone.", call. = FALSE)

  # 3. Only one of the
  if (any(!(nam %in% c("edge", "alter", "ego"))))
    stop("Invalid term.", call. = FALSE)

  # 4. Checking if it is linear or an interaction
  linear <- ifelse(ncol(mat) == 1, TRUE, FALSE)

  # Applying formulas
  # Mofiying the likelihood function and the parameters for the mcmc
  val <- attr(tm, "variables")

  # Space where we will save the parameters
  par <- new.env()

  for (i in 2:length(val))
    if (!is.call(val[[i]])) {
      eval(
        call(as.character(val[[i]]), x = x, env = par)
      )
    } else {
      val[[i]]$x   <- bquote(x)
      val[[i]]$env <- bquote(par)
      eval(val[[i]])
    }

  # Creating the coloring
  col <- do.call(new_edge_coloring, as.list(par))

  lapply(seq_len(x$.M), function(e) {

    nam <- netplot_name$make(x$.edgelist[e,])

    col$vertex(
      e = e,
      i = x$.edgelist[e, 1],
      j = x$.edgelist[e, 2],
      n = length(x$children$graph$children[[nam]]$children$line$id.lengths)
    )

  })
}

#' Formulas in `netplot`
#'
#' Edge colors in both [nplot()] and [set_edge_gpar()] can be specified using
#' a formula based on `ego()` and `alter()` (source and target). This way the
#' user can set various types of combination vaying the mixing of the colors,
#' the alpha levels, and the actual mixing colors to create edge colors.
#'
#' @param col Any valid color. Can be a single color or a vector.
#' @param alpha Number. Alpha levels
#' @param mix Number. For mixing colors between `ego` and `alter`
#' @param env,type,postfix For internal use only.
#' @param ... Passed to `color_formula`.
#' @param x An object of class [netplot].
#' @examples
#' if (require(gridExtra) & require(magrittr)) {
#'   library(igraph)
#'   net <- make_ring(4)
#'
#'   set.seed(1)
#'   np <- nplot(net, vertex.color = grDevices::hcl.colors(4), vertex.size.range=c(.1, .1))
#'   np %<>% set_edge_gpar(lwd = 4)
#'
#'   grid.arrange(
#'     np,
#'     np %>% set_edge_gpar(col =~ego + alter),
#'     np %>% set_edge_gpar(col =~ego(alpha=0) + alter),
#'     np %>% set_edge_gpar(col =~ego + alter(alpha=0)),
#'     np %>% set_edge_gpar(col =~ego(mix=0) + alter(mix=1)),
#'     np %>% set_edge_gpar(col =~ego(mix=1) + alter(mix=0))
#'   )
#' }
#' @name netplot-formulae
#' @export
color_formula <- function(x, col, alpha, env, type, mix = 1, postfix = NULL) {

  n <- switch(type, vertex = x$.N, edge = x$.M)

  # Checking color
  col <- if (missing(col))
    get_vertex_gpar(x, "core", "fill")$fill
  else if (length(col) == 1)
    rep(col, x$.N)
  else if (length(col) != n)
    stop("`col` has the wrong length (", length(col), "). When passing a ",
         "vector it should be of length ", n, ".", call. = FALSE)

  # Checking alpha
  alpha <- if (missing(alpha))
    rep(.8, n)
  else if (length(alpha) == 1)
    rep(alpha, n)
  else if (length(alpha) != n)
    stop("`alpha` has the wrong length (", length(alpha), "). When passing a ",
         "vector it should be of length ", n, ".", call. = FALSE)

  # Checking mixing levels
  mix <- if (missing(mix))
    rep(1, x$.M)
  else if (length(mix) == 1)
    rep(mix, x$.M)
  else if (length(mix) != x$.M)
    stop("`mix` has the wrong length (", length(mix), "). When passing a ",
         "vector it should be of length ", x$.M, ".", call. = FALSE)

  # Assigning values
  if (type == "vertex") {
    env[[paste0("vertex_color_", postfix)]] <- col
    env[[paste0("vertex_alpha_", postfix)]] <- alpha
  } else {
    env[["edge_color"]] <- col
    env[["edge_alpha"]] <- alpha
  }

  if (!length(env$mix))
    env$mix <- mix
  else {
    env$mix <- 1 - env$mix/(env$mix + mix)
  }

  invisible()
}

#' @export
#' @rdname netplot-formulae
ego   <- function(...) {

  dots        <- list(...)
  dots$type   <- "vertex"
  dots$postfix <- "i"

  do.call(color_formula, dots)
}

#' @export
#' @rdname netplot-formulae
alter <- function(...) {

  dots        <- list(...)
  dots$type   <- "vertex"
  dots$postfix <- "j"

  do.call(color_formula, dots)

}

edge <- function(x, col, alpha = .8) {

  # obtain default
  col <- if (missing(col))
    get_edge_gpar(x, "line", "col")$col
  else if (length(col) == 1)
    rep(col, x$.M)
  else if (length(col) != x$.M)
    stop("`col` has the wrong length (", length(col), "). When passing a ",
         "vector it should be of length ", x$.M, ".", call. = FALSE)

  # Checking alpha
  alpha <- if (missing(alpha))
    rep(.8, x$.M)
  else if (length(alpha) == 1)
    rep(alpha, x$.M)
  else if (length(alpha) != x$.M)
    stop("`alpha` has the wrong length (", length(alpha), "). When passing a ",
         "vector it should be of length ", x$.M, ".", call. = FALSE)


  list(
    col   = col,
    alpha = alpha
  )
}

# netplot_edge_formulae(~ego:alter)
