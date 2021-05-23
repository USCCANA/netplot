#' A faster implementation of [grDevices::colorRamp] for linear interpolation.
#'
#' @param x A vector of colors.
#' @param alpha Logical scalar. When `TRUE`
#' This implementation of `colorRamp` can be 2 or more times faster than the `grDevices`
#' version. It is intended for consecutive calls (i.e. in a loop) to improve
#' performance. It is equivalent to the linear interpolation of the function
#' `colorRamp`.
#' @param thresholds A numeric vector of length `length(x)`. Optional threshold
#' levels so that the mixing can be different that even.
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
#' # Another example setting different thresholds
#' myf  <- colorRamp2(c("black", "steelblue"))
#' myf2 <- colorRamp2(c("black", "steelblue"), thresholds=c(0, .7))
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
#'   col = rgb(myf2((1:10)/10), maxColorValue = 255)
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
colorRamp2 <- function(x, alpha = TRUE, thresholds=NULL) {

  # Converting to RGB
  col <- t(grDevices::col2rgb(col = x, alpha = alpha))

  # Defining thresholds for the linear combinations
  n <- length(x)

  if (!length(thresholds))
    thresholds <- seq(0, 1, length.out = n)
  else if (length(thresholds) != n)
    stop("The length of `thresholds` should be the same as `length(n)`.",
         call. = FALSE)

  if (n == 2L) {
    function(x) {
      N    <- length(x)
      col0 <- rep(1, N)
      x    <- (x - thresholds[1])/(thresholds[2] - thresholds[1])
      x[x<0] <- 0
      x[x>1] <- 1

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

#' Draw segments colores by gradients
#' @param x,y Coordinates passed to [grDevices::xy.coords].
#' @param col Color ramp function (see [grDevices::colorRamp]).
#' @param lend Passed to [graphics::segments].
#' @param ... Further arguments passed to `segments`.
#' @export
#' @examples
#'
#' set.seed(1)
#' x <- cbind(cumsum(rnorm(1e3, sd=.1)), cumsum(rnorm(1e3, sd=.4)))
#' plot(x, type="n")
#' segments_gradient(x)
segments_gradient <- function(
  x, y = NULL,
  col  = colorRamp2(c("transparent", "black"), TRUE),
  lend = 1,
  ...) {

  # Getting new coords
  x   <- grDevices::xy.coords(x, y)
  n   <- length(x$x) - 1L
  col <- col(0:(n-1)/(n-1))
  col <- rgb(col, alpha=col[,4], maxColorValue = 255)

  graphics::segments(
    x0 = x$x[1:n],
    y0 = x$y[1:n],
    x1 = x$x[2:(n+1)],
    y1 = x$y[2:(n+1)],
    col = col,
    lend = lend,
    ...
  )

}
