new_coloring <- function(
  vertex_color_i,
  vertex_color_j,
  vertex_alpha_i,
  vertex_alpha_j,
  mix
  ) {

  function(e, i, j, n) {

    # Adjusting alpha levels
    col_i <- grDevices::adjustcolor(vertex_color_i[i], alpha.f = vertex_alpha_i[i])
    col_j <- grDevices::adjustcolor(vertex_color_i[j], alpha.f = vertex_alpha_j[j])

    # Linear combination between i and j
    mix <- mix[e]
    mix <- polygons::colorRamp2(c(col_i, col_j))(mix)

    # Returning
    grDevices::rgb(mix, alpha = mix[,4], maxColorValue = 255)

  }


}

# N <- 20
# M <- 50
# X <- new_coloring(
#   viridis::viridis(N),
#   viridis::viridis(N),
#   rep(.5, N),
#   rep(.8, N),
#   rep(.5, M)
#   )
#
# # Updating to set a path from i to j
# barplot(1:5, col = X$col(4, 1, 1, 5))
# body(X$col)[[4]] <- bquote(mix <- seq(0, 1, length.out = n))
# barplot(1:5, col = X$col(4, 1, 1, 5))
#
# exists(as.character(substitute(x)))
#
# netplot_formulas <- new.env()
# netplot_formulas$ego <- function(x, alpha, prop, coloring) {
#
#   if (1 && is.character(x))
#     return(1)
#
#   0
#
# }
#
# body(X$col)[[2]][[3]] <- "blue"
# X
#
# netplot_formulas$ego(a)

#' @noRd
#' @importFrom stats terms
netplot_edge_formulae <- function(fm) {

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
  if (ncol(mat) == 1)
    1
}

ego <- function() {

}

alter <- function() {

}

edge <- function() {

}

netplot_edge_formulae(~ego:alter)
