#' n-sided polygons
#' Calculate the coordinates for an nsided polygon
#' @param x,y Numeric scalar. Origin of the polygon.
#' @param n Integer scalar. Number of sides.
#' @param r Numeric scalar. Radious of the polygon.
#' @param d Numeric scalar. Starting degree in radians.
#' @return
#' A two column matrix with the coordinates to draw a n sided polygon.
#' @examples
#' graphics.off()
#' oldpar <- par(no.readonly = TRUE)
#'
#' par(xpd = NA, mfrow = c(3, 3), mai = rep(0, 4))
#' for (n in c(2, 3, 4, 5, 6, 8, 12, 20, 50)) {
#'
#'   plot.new()
#'   plot.window(c(-1.25,1.25), c(-1.25,1.25))
#'
#'   for (i in seq(1, .0005, length.out = 200)) {
#'     col <- adjustcolor("tomato", alpha.f = i)
#'     polygon(npolygon(x=(i-1)/4, y = (i-1)/4, r = i, d = i-1, n = n),
#'             col = NA, border=col)
#'   }
#'
#'   mtext(sprintf("n = %i", n), side = 1, line = -3)
#' }
#'
#' par(oldpar)
#' @export
npolygon <- function(
  x = 0,
  y = 0,
  n = 6L,
  r = 1.0,
  d = 2.0*pi/(n)/2
) {

  deg <- seq(d, 2.0*pi + d, length.out = n + 1)[-(n+1)]

  coords <- cbind(
    x = cos(deg)*r + x,
    y = sin(deg)*r + y
  )

}


#' Function to rescale a polygon such that it keeps the "original" aspect ratio.
#' @param coordinates A two-column matrix or data frame with coordinates (xy).
#' @param yorigin Reference point in the y-axis.
#' @param adj Rescale factor (scalar). By default the adjustment is computed
#' using `graphics::par()` `usr` and `pin`.
# @export
#' @noRd
rescale_polygon <- function(
  coordinates,
  yorigin = mean(coordinates[,2]),
  adj = NULL
) {

  # Adjustment value
  if (!length(adj)) {
    usr_adj <- with(graphics::par(), (usr[2] - usr[1])/(usr[4] - usr[3]))
    dev_adj <- with(graphics::par(), pin[2]/pin[1])
    adj <- 1/dev_adj/usr_adj
  }

  # If it is multiple polygons (adj is passed by scoping)
  if (!is.data.frame(coordinates) && is.list(coordinates))
    return(mapply(rescale_polygon, coordinates=coordinates, yorigin=yorigin,
                  SIMPLIFY = FALSE))

  # Adjusting
  if ((is.matrix(coordinates) | is.data.frame(coordinates)) && ncol(coordinates) > 1)
    coordinates[,2] <- yorigin + (coordinates[,2] - yorigin)*adj
  else
    return(yorigin + (coordinates - yorigin)*adj)

  coordinates
}
