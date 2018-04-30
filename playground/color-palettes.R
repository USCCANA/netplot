#' A faster implementation of [grDevices::colorRamp] for linear interpolation.
#'
#' @param A vector of colors.
#' @param alpha Logical scalar. When `TRUE`
#' This implementation of `colorRamp` can be 2 or more times faster than the `grDevices`
#' version. It is intended for consecutive calls (i.e. in a loop) to improve
#' performance. It is equivalent to the linear interpolation of the function
#' `colorRamp`.
#' @export
#'
#' @examples
#'
#' # Creating a function for 2 colors
#' myf <- colorRamp2(c("black", "steelblue"))
#' f   <- colorRamp(c("black", "steelblue"))
#'
#' plot.new()
#' plot.window(xlim = c(0,2), ylim = c(1, 11))
#'
#' # These should be the same colors
#' rect(
#'   xleft   = 0,
#'   xright  = 1,
#'   ybottom = 1:10,
#'   ytop    = 2:11,
#'   col = rgb(myf((1:10)/10), maxColorValue = 255)
#'   )
#' rect(
#'   xleft   = 1,
#'   xright  = 2,
#'   ybottom = 1:10,
#'   ytop    = 2:11,
#'   col = rgb(f((1:10)/10), maxColorValue = 255)
#' )
#'
#'
#' \dontrun{
#' # Ho much better?
#' n <- 20
#' microbenchmark::microbenchmark(
#'   myramp = myf((1:n)/n),
#'   rramp  = f((1:n)/n), times = 1e4,
#'   unit = "relative"
#' )
#' }
#'
colorRamp2 <- function(x, alpha = TRUE) {

  # Converting to RGB
  col <- t(grDevices::convertColor(col = x, alpha = alpha))

  # Defining thresholds for the linear combinations
  n          <- length(x)
  thresholds <- seq(0, 1, length.out = n)

  if (n == 2L) {
    function(x) {
      N    <- length(x)
      col0 <- rep(1, N)
      col[col0, ,drop=FALSE] * (1-x) + col[col0+1, ,drop=FALSE] * x
    }
  } else
    function(x) {

      # Which colors

      col0 <- findInterval(x, thresholds)
      col1 <- col0 + 1*(col0 < n)

      # Adjusting levels
      x <- (x - thresholds[col0])/(thresholds[col1] - thresholds[col0] + 1e-20)

      col[col0, , drop=FALSE] * (1-x) +
        col[col1, , drop=FALSE] * x

    }

}



