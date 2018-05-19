rotate <- function(mat, origin, alpha) {

  R <- matrix(c(cos(alpha), -sin(alpha), sin(alpha), cos(alpha)), nrow=2, ncol=2)

  origin <- matrix(origin, ncol=2, byrow = TRUE, nrow=nrow(mat))
  ((mat - origin) %*% R) + origin
}
