#' Rescale the size of a node to make it relative to the aspectt ratio of the device
#' @param size Numeric vector. Size of the node (radious).
#' @param rel Numeric vector of length 3. Relative size for the minimum and maximum
#' of the plot, and curvature of the scale. The third number is used as `size^rel[3]`.
#'
#' @details
#' This function is to be called after [plot.new], as it takes the parameter `usr`
#' from the
#' @noRd
rescale_size <- function(size, rel=c(.01, .05, 1)) {

  # Checking the rel size
  if (length(rel) == 2)
    rel <- c(rel, 1)
  else if (length(rel) > 3) {
    warning("`rel` has more than 3 elements. Only the first 3 will be used.")
  } else if (length(rel) < 2) {
    stop("`rel` must be at least of length 2 and at most of length 3.")
  }

  # Creating curvature
  size <- size^rel[3]

  # Rescaling to be between range[1], range[2]
  sran <- range(size, na.rm=TRUE)

  if ((sran[2] - sran[1]) > 1e-5)
    size <- (size - sran[1])/(sran[2] - sran[1]) # 0-1
  else
    size <- size/sran[1]

  size * (rel[2] - rel[1]) + rel[1]

}

#' Adjust coordinates to fit aspectt ratio of the device
#' @param coords Two column numeric matrix. Vertices coordinates.
#' @details
#' It first adjusts `coords` to range between `-1,1`, and then, using
#' `graphics::par("pin")`, it rescales the second column of it (`y`) to adjust
#' for the device's aspect ratio.
#' @param adj Numeric vector of length 2.
#' @noRd
# @export
fit_coords_to_dev <- function(coords, adj = grDevices::dev.size()) {

  # Making it -1 to 1
  yran <- range(coords[,2], na.rm = TRUE)
  xran <- range(coords[,1], na.rm = TRUE)

  coords[,1] <- (coords[,1] - xran[1])/(xran[2] - xran[1])*2 - 1
  coords[,2] <- (coords[,2] - yran[1])/(yran[2] - yran[1])*2 - 1

  # Adjusting aspectt ratio according to the ploting area
  coords[,2] <- coords[,2]*adj[2]/adj[1]

  # Returning new coordinates
  coords

}

#' Rotation of polygon
#' @param mat Two-column numeric matrix. Coordinates of the polygon.
#' @param origin Numeric vector of length two. Origin.
#' @param alpha Numeric scalar. Rotation degree in radians.
#' @noRd
# @export
rotate <- function(mat, origin, alpha) {
  R <- matrix(
    c(cos(alpha), -sin(alpha), sin(alpha), cos(alpha)),
    nrow = 2, byrow = TRUE)

  origin <- matrix(c(origin[1], origin[2]), ncol=2, nrow = nrow(mat), byrow = TRUE)
  t(R %*% t(mat - origin)) + origin
}


#' Arc between two nodes
#'
#' @param p0,p1 Numeric vector of length 2. Center coordinates
#' @param alpha Numeric scalar. Arc angle in radians.
#' @param n Integer scalar. Number of segments to approximate the arc.
#' @param radii Numeric vector of length 2. Radious
#' @noRd
# @export
#'
arc <- function(
  p0,
  p1,
  alpha = pi/3,
  n     = 10L,
  radii = c(0, 0)
) {

  # If no curve, nothing to do (old fashioned straight line)
  if (alpha == 0 | n == 1) {
    alpha <- 1e-5
  }

  elevation <- atan2(p1[2]-p0[2], p1[1] - p0[1])

  # Constants
  d <- sqrt(sum((p0 - p1)^2))

  # If overlapping, then fix the radius to be the average
  if ((d - sum(radii)) < 0) {
    r <- mean(radii)
    alpha <- 2*pi - asin(d/2/r)*2
  } else {
    r <- d/2/(sin(alpha/2))
  }

  # Angles
  alpha0 <- asin(radii[1]/2/r)*2
  alpha1 <- asin(radii[2]/2/r)*2

  # Angle range
  alpha_i <- seq(
    pi/2 + (alpha/2 - alpha0) ,
    pi/2 - (alpha/2 - alpha1),
    length.out = n + 1
  )

  # Middle point
  M <- c(
    p0[1] + d/2,
    p0[2] - cos(alpha/2)*r
  )

  ans <- cbind(
    M[1] + cos(alpha_i)*r,
    M[2] + sin(alpha_i)*r
  )


  # Rotation and return
  ans <- rotate(ans, p0, elevation)

  # Separating the segments
  ans <- cbind(
    as.vector(t(cbind(ans[-(n + 1),1], ans[-1,1]))),
    as.vector(t(cbind(ans[-(n + 1),2], ans[-1,2])))
  )

  structure(
    ans,
    alpha0 = atan2(ans[1,2] - p0[2], ans[1,1] - p0[1]),
    alpha1 = atan2(p1[2] - ans[n*2,2], p1[1] - ans[n*2,1]),
    midpoint = ans[ceiling(n/2),]
  )

}


#' Arrow polygon.
#'
#' @param x Numeric vector of length 2. Coordinates of the tip
#' @param alpha,l,a,b Numeric scalars
#' @noRd
# @export
arrow_fancy <- function(x, alpha = 0, l=.25, a=pi/6, b = pi/1.5) {


  p_left  <- x + c(-cos(a), sin(a))*l

  base <- l*sin(a)
  base2 <- base * cos(pi - b)/sin(pi - b)
  p_mid   <- p_left + c(base2, -base)

  p_right <- x - c(cos(a), sin(a))*l

  ans <- rbind(x, p_left, p_mid, p_right)

  # Rotation

  rotate(ans, x, alpha = alpha)


}

#' Takes a graph attribute and maps it to a shape
#' @param x Is (any) vector to be mapped.
#' @return 
#' A vector of shapes (encoded as integers) from 2 to 8.
#' @noRd
map_attribute_to_shape <- function(x) {

  # Get the unique values of x, if it is more than 9
  # then we will throw an error.
  nunique <- length(unique(x))
  if (nunique > length(sides_lookup)) {
    stop(
      "The number of unique values of the attribute is greater than the number of shapes available. ",
      "Please, use a different attribute or a different shape."
    )
  }

  # If the variable already contains the values
  if (all(x %in% names(sides_lookup)))
    return(x)

  # We can now turn this x into a factor, and then
  # numbers to be used as indices
  names(sides_lookup)[as.numeric(factor(x))]

}

