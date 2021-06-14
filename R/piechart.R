pieslice <- function(a0, a1, r, d, x0, y0, edges, off) {

  # Intermideate points
  mid <- seq(a0, a1, by = 2*pi/edges*sign(a1 - a0))

  # In case that the points are not sufficient
  if (length(mid) < 3)
    mid <- seq(a0, a1, length.out = 3)
  if (utils::tail(mid, 1) != a1)
    mid <- seq(a0, a1, length.out = length(mid))

  # Computing midpoints
  mid <- cbind(cos(mid), sin(mid))
  m <- nrow(mid)

  if (d != 0) {
    pbase <- (mid*d)[m:1,]
  } else pbase <- c(0,0)

  ans <- rbind.data.frame(
    mid*r,
    pbase,
    make.row.names=FALSE
  )

  # Translating to the origin
  ans[,1] <- x0 + ans[,1]
  ans[,2] <- y0 + ans[,2]

  # Setting off
  ans[,1] <- ans[,1] + cos((a1 + a0)/2)*off
  ans[,2] <- ans[,2] + sin((a1 + a0)/2)*off

  colnames(ans) <- c("x", "y")
  ans


}

#' Computes the coordinates to plot a circle
#'
#' It actually does it by creating a polygon with 100 edges.
#' @param x0,y0 Numeric scalars. Center of the circle
#' @param r Numeric scalar. Radius of the circle.
#' @param rescale Logical scalar. When [TRUE], rescales the polygon
#' coordinates such that it preservers its aspectt ratio once plotted.
#'
#'@noRd
# @export
circle <- function(x0, y0, r, rescale=TRUE) {
  ans <-  pieslice(0, 2*pi, r=r, d=0, x0, y0, edges=100, off=0)
  ans <- ans[-nrow(ans), ]

  if (rescale)
    ans <- rescale_polygon(ans, y0)

  ans
}

#' A flexible piechart.
#'
#' While similar to [graphics::pie()], this function is much more
#' flexible as it allows providing different parameters for each slice of the pie.
#' Furthermore, it allows adding the plot to the current device, making it possible
#' to create compound piecharts.
#'
#' @param x Numeric vector. Values that specify the area of the slices.
#' @param add Logical scalar. When `TRUE` it is added to the current device.
#' @param radius Numeric vector. Radious of each slice (can be a scalar).
#' @param doughnut Numeric scalar. Radious of each inner circle (doughnut) (can be a scalar).
#' @param origin Numeric vector of length 2. Coordinates of the origin.
#' @param edges Numeric scalar. Smoothness of the slices curve (can be a vector).
#' @param slice.off Numeric vector. When `!=0`, specifies how much to
#' move the slice away from the origin. When scalar is recycled.
#' @param labels Character vector of length `length(x)`. Passed to
#' [graphics::text()].
#' @param tick.len Numeric scalar. Size of the tick marks as proportion of the radius.
#' @param text.args List. Further arguments passed to [graphics::text()].
#' @param segments.args List. Further arguments passed to [graphics::segments()]
#' when drawing the tickmarks.
#' @param init.angle Numeric scalar. Angle from where to start drawing in degrees.
#' @param last.angle Numeric scalar. Angle where to finish drawing in degrees.
#' @param skip.plot.slices Logical scalar. When `FALSE`, slices are not drawn.
#' This can be useful if, for example, the user only wants to draw the labels.
#' @param rescale Logical scalar. When `TRUE` (default), the y-coordinates of
#' the polygons (slices), text and tickmarks will be rescaled such that the
#' aspectt ratio is preserved, i.e. looks like a circle.
#' @param ... Further arguments passed to [graphics::polygon()]
#' (see details).
#'
#' @return
#' A list with the following elements:
#' \item{slices}{A list of length `length(x)` with the coordinates of each
#'   slice.}
#' \item{textcoords}{A numeric matrix of size `length(x)*2` with
#'   coordinates where the labels can be put at.}
#' \item{alpha0}{A numeric vector of size `length(x)` with the starting
#' degree in radians of the slice.}
#' \item{alpha1}{A numeric vector of size `length(x)` with the ending
#' degree in radians of the slice.}
#'
#' @details The function is a wrapper of [graphics::polygon()],
#' so all parameters such as color, density, border, etc. are passed directly
#' by [mapply()] so that are specified one per slice. The coordinates
#' of the slices are computed internally.
#'
#'
#' @seealso https://commons.wikimedia.org/wiki/File:Nightingale-mortality.jpg
#' @export
#' @examples
#'
#' # Example 1 -----------------------------------------------------------------
#' # A set of 3 nested rings rings starting at 315 deg. and ending at 270 deg.
#'
#' # Values to plot
#' vals <- c(1,2,3,10)
#'
#' # Outer (includes labels)
#' piechart(vals, col=grDevices::blues9[5:8], border=NA, doughnut = .5,
#'     radius=.75, labels=vals, init.angle = 315, last.angle = 270)
#'
#' # Middle
#' piechart(vals, col=grDevices::blues9[3:6], border=NA, doughnut = .3,
#'     radius=.5, add=TRUE, init.angle = 315, last.angle = 270)
#'
#' # Inner
#' piechart(vals, col=grDevices::blues9[1:4], border="gray", doughnut = .1,
#'     radius=.3, add=TRUE, init.angle = 315, last.angle = 270)
#'
#' # Example 2 -----------------------------------------------------------------
#' # Passing values to polygon and playing with the radius and slice.off
#'
#' piechart(1:10, density=(1:10)^2/2, slice.off = (1:10)/30, doughnut = .5,
#'   radius = sqrt(10:1),
#'   # Here we are setting random labels...
#'   labels=sapply(1:10, function(x) paste(sample(letters, x, TRUE), collapse=""))
#'   )
#'
piechart <- function(
  x,
  labels = names(x),
  radius = 1,
  doughnut = 0,
  origin = c(0,0),
  edges = 200,
  slice.off = 0,
  init.angle = 0,
  last.angle = 360,
  tick.len = .1,
  text.args = list(),
  segments.args = list(),
  skip.plot.slices=FALSE,
  add = FALSE,
  rescale = TRUE,
  ...) {

  # X must be numeric
  if (!is.numeric(x))
    stop("-x- must be numeric")
  x <- tryCatch(as.vector(x), error = function(e) e)
  if (inherits(x, "error"))
    stop("Coercion of -x- as vector failed:\n", x)

  # Assigning alpha
  init.angle <- init.angle/360*2.0*pi # as radians
  last.angle <- last.angle/360*2.0*pi

  alpha1 <- cumsum(x/sum(x)*ifelse(
    init.angle >= last.angle, 2*pi - init.angle + last.angle,
    last.angle - init.angle
  )) + init.angle
  alpha0 <- c(init.angle, alpha1[-length(x)])

  ans <- mapply(
    pieslice,
    a0 = alpha0,
    a1 = alpha1,
    r = radius, d=doughnut, x0=origin[1], y0=origin[2],
    edges = edges,
    off = slice.off,
    SIMPLIFY = FALSE
  )

  # Fetching size
  cex <- if (length(labels) && ("cex" %in% names(text.args)))
    text.args[["cex"]]
  else 1

  # Creating the device
  maxradius <- max(radius)
  if (!add) {
    graphics::plot.new()

    # Adjusting so that we get nice circles
    adj <- graphics::par()$pin
    adj <- adj[1]/adj[2]

    # Adjusting the size... including the labels
    ran <- if (length(labels))
      max(
        c(
          graphics::strwidth(labels, units="user", cex=cex),
          graphics::strheight(labels, units="user", cex=cex)
        ),
        na.rm=TRUE)*2
    else 0

    ran <- (ran + maxradius*1.1 + tick.len/2 + max(slice.off))*c(-1,1)

    # Adjustment depending on the asp
    if (adj > 1)
      graphics::plot.window(xlim=ran*adj, ylim = ran)
    else
      graphics::plot.window(xlim=ran, ylim = ran*adj)
  }

  # Adjusting the polygon
  if (rescale)
    ans <- rescale_polygon(ans, origin[2])

  # Adding the slices
  if (!skip.plot.slices)
    mapply(graphics::polygon,
           x = lapply(ans, "[", j=1, i=),
           y = lapply(ans, "[", j=2, i=),
           ..., SIMPLIFY = FALSE
    )

  # Midpoints
  angles     <- (alpha0 + alpha1)/2
  textcoords <- cbind(
    origin[1] + cos(angles)*(radius*1.05 + tick.len/2 + slice.off) ,
    origin[2] + sin(angles)*(radius*1.05 + tick.len/2 + slice.off)
  )

  # If labels are passed
  if (length(labels)) {

    # Adjusting according to string lenght
    textcoords <- textcoords +
      cbind(
        cos(angles)*graphics::strwidth(labels, cex=cex)/2,
        sin(angles)*graphics::strwidth(labels, cex=cex)/2
      )

    if (rescale)
      textcoords <- rescale_polygon(textcoords, origin[2])

    # Drawing the text
    do.call(mapply,
            c(list(FUN = graphics::text,
                   x=textcoords[,1],
                   y=textcoords[,2],
                   labels=labels,SIMPLIFY = FALSE),
              text.args
            ))


    # Here should go the tick marks...
    x0 <- origin[1] + cos(angles)*(radius - tick.len/2 + slice.off)
    x1 <- origin[1] + cos(angles)*(radius + tick.len/2 + slice.off)
    y0 <- origin[2] + sin(angles)*(radius - tick.len/2 + slice.off)
    y1 <- origin[2] + sin(angles)*(radius + tick.len/2 + slice.off)

    if (rescale) {
      y0 <- rescale_polygon(y0, origin[2])
      y1 <- rescale_polygon(y1, origin[2])
    }


    toplot <- which(!is.na(labels))
    do.call(graphics::segments,
            c(
              list(
                x0 = x0[toplot],
                y0 = y0[toplot],
                x1 = x1[toplot],
                y1 = y1[toplot]
              ),
              segments.args
            ))

  }


  # Returning
  invisible(
    list(
      slices     = ans,
      textcoords = textcoords,
      alpha0     = alpha0,
      alpha1     = alpha1
    )
  )

}


#' Function to create a color key
#'
#' @param x0,x1,y0,y1 Numeric scalars. Coordinates of the lower left and
#' upper right points where the color key will be drawn as proportion of
#' the plotting region.
#' @param label.from,label.to Character scalar. Labels of the lower and
#' upper values of the color key.
#' @param tick.range,tick.marks Numeric vectors specifying the range and
#' the tickmarks respectively.
#' @param cols Character scalar. Colors specifications to create the
#' color palette.
#' @param nlevels Integer scalar. Number of levels to extrapolate.
#' @param main Character scalar. Title of the colorkey.
#' @param relative Logical scalar. When `TRUE` the color key is drawn relative
#' to the plotting region area taking `x0, x1, y0, y1` as relative location.
#' @param tick.args,label.args,main.args Lists of arguments passed to
#' [graphics::text] for drawing ticks, labels and main respectively.
#' @export
#' @return NULL.
#' @examples
#' set.seed(22231)
#'
#' # A random figure
#' dat <- matrix(runif(100*3), ncol = 3)
#' col <- colorRamp2(c("blue", "white", "red"))
#'
#' plot(
#'   dat[,1], dat[,2],
#'   col = rgb(col(dat[,3]), maxColorValue=255),
#'   cex=2, pch=20
#'   )
#'
#' # Pretty color key
#' colorkey(
#'   x0 = .60, y0 = .80,
#'   x1 = .95, y1 = .95,
#'   cols = c("blue", "white", "red"),
#'   main = "Some color scale"
#' )
colorkey <- function(
  x0,y0,x1,y1,
  cols       = c("white", "steelblue"),
  tick.range = c(0, 1),
  tick.marks = seq(tick.range[1], tick.range[2], length.out = 5L),
  label.from = NULL,
  label.to   = NULL,
  nlevels    = 100,
  main       = NULL,
  relative   = TRUE,
  tick.args  = list(),
  label.args = list(),
  main.args  = list()
) {

  # If relative (the default, will be added to )
  if (relative) {
    usr <- graphics::par("usr")
    xpd <- graphics::par("xpd")
    on.exit({
      graphics::plot.window(
        usr[1:2],
        usr[3:4],
        new=FALSE,
        xpd=xpd,
        xaxs="i",
        yaxs="i"
      )
    })
    graphics::plot.window(c(0,1), c(0,1), new=FALSE, xpd=NA)

  }

  # Adjusting to textsize
  x0 <- x0 + graphics::strwidth(label.from)
  x1 <- x1 - graphics::strwidth(label.to)
  y0 <- y0 + max(graphics::strheight(tick.marks))
  y1 <- y1 - graphics::strheight(main)

  # Writing labels
  do.call(
    graphics::text,
    c(list(
      x      = c(
        x0 - graphics::strwidth(label.from)/2,
        x1 + graphics::strwidth(label.to)/2
      ),
      y      = rep((y1+y0)/2, 2),
      labels = c(label.from, label.to)
    ), label.args)
  )

  # Readjusting for more space
  x0 <- x0 + (x1-x0)/40
  x1 <- x1 - (x1-x0)/40

  # Computing coordinates
  cols <- grDevices::colorRampPalette(cols)(nlevels)
  n  <- length(cols)
  xsize <- (x1 - x0)/n

  xcoords <- seq(x0 + xsize/2, x1 - xsize/2, length.out = n)
  ycoords <- rep((y1+y0)/2, n)

  # xcoords[1] <- xcoords[1] + xsize

  # Drawing rectangles
  graphics::symbols(
    x          = xcoords,
    y          = ycoords,
    inches     =FALSE,
    bg         = cols,
    fg         = "transparent",
    rectangles = cbind(rep(xsize, n), y1-y0),
    add        = TRUE
  )

  # Bouding box
  graphics::symbols(
    x = (x1 + x0)/2,
    y = (y1 + y0)/2,
    rectangles = cbind((x1 - x0), y1 - y0),
    add=TRUE, inches = FALSE
  )

  # Top tickmarks
  tick.pos <- (tick.marks - tick.range[1])/(tick.range[2] - tick.range[1])*(x1-x0) + x0
  graphics::segments(
    x0 = tick.pos,
    y0 = y0 - (y1 - y0)/5 ,
    x1 = tick.pos,
    y1 = y0 + (y1 - y0)/5
  )

  do.call(
    graphics::text,
    c(
      list(
        x = tick.pos,
        y = y0 - (y1 - y0)/5 - graphics::strheight(max(tick.marks, na.rm=TRUE))/1.5,
        labels = tick.marks
      ), tick.args))

  if (length(main))
    do.call(
      graphics::text,
      c(list(
        x = (x1 + x0)/2,
        y = y1,
        pos = 3,
        labels = main
      ), main.args)
    )

  invisible()
}

