#' @keywords internal
color_nodes <- function(...) UseMethod("color_nodes")

#' @keywords internal
color_nodes.formula <- function(formula, ...) {

  # Extract the LHS of the formula
  lhs <- as.character(formula[[2]])

  # Extract the RHS of the formula
  rhs <- as.character(formula[[3]])

  # Check if the lhs exists in the parent environment
  if (!(lhs %in% ls(parent.frame()))) {
    stop("LHS does not exist in the parent environment")
  }

  # Check if the rhs is a valid name
  color_nodes(get(lhs, envir = parent.frame()), rhs, ...)

}

#' @keywords internal
color_nodes.default <- function(
  graph,
  attribute,
  palette = grDevices::palette(),
  na_color = "white",
  ...
  ) {

  # Extracting the attribute from the graph
  value <- get_vertex_attribute(graph, attribute)
  attr_type <- class(value)

  # Identifying NAs
  na_idx <- which(is.na(value))

  # Handle characters, are turned into factors
  if (attr_type == "character") {

    value <- as.factor(value)
    attr_type <- "factor"

  } else if (inherits(value, "numeric")) {

    # Checking if it is numeric, but if it can be converted to int
    test_int <- abs(as.integer(value) - value) < .Machine$double.eps^.5

    if (all(test_int, na.rm = TRUE)) {

      value <- as.integer(value)
      attr_type <- "integer"

    }

  }

  

  # Saving the original
  value_orig <- value

  # Handle factors
  if (attr_type == "factor") {

    # Map levels to colors
    cpal  <- grDevices::colorRampPalette(palette)(nlevels(value))

    # Creating mapping to recover colors
    names(cpal) <- levels(value)

    value <- cpal[as.integer(value)]

  }  else if ("numeric" %in% attr_type) { # Handle numerics

    # Find min and max
    attr_min <- min(value)
    attr_max <- max(value)

    # Create color scale
    value <- grDevices::colorRamp(palette)(
      (value - attr_min)/(attr_max - attr_min)
    )

    cpal <- function(val) {grDevices::rgb(
      grDevices::colorRamp(palette)(val),
      maxColorValue = 255
    )}

    # Color nodes based on attribute value
    value <- grDevices::rgb(value, maxColorValue = 255)

  } else if ("logical" %in% attr_type) { # Handle logicals

    # Creating mapping to recover colors
    cpal <- palette[1:2]
    names(cpal) <- c("FALSE", "TRUE")

    # Color nodes
    value <- cpal[as.integer(value) + 1]
  
  } else if ("integer" %in% attr_type) {
      
      # Find min and max
      attr_min <- min(value, na.rm = TRUE)
      attr_max <- max(value, na.rm = TRUE)
  
      # Create color scale
      cpal <- grDevices::colorRampPalette(palette)(
        length(attr_min:attr_max)
      )
  
      names(cpal) <- as.character(c(attr_min:attr_max))
  
      # Color nodes based on attribute value
      value <- cpal[as.character(value)]

  }

  # Handle other types (characters, dates)
  else {
    stop("Attribute type not supported")
  }

  value[na_idx] <- na_color

  structure(
    value,
    class     = "netplot_color_nodes",
    attr_type = attr_type,
    palette   = palette,
    na_color  = na_color,
    cpal      = cpal,
    value     = value_orig,
    attr_name = attribute
  )

}

#' @noRd 
#' @importFrom stats quantile
#' @keywords internal
color_nodes_legend <- function(object) {

  # Extracting the fill legend
  x <- object$.legend_vertex_fill

  if (!length(x))
    return(invisible(NULL))

  if (!inherits(x, "netplot_color_nodes")) {
    stop("Object is not of class netplot_color_nodes")
  }

  # Continuous attributes get a color bar rather than a set of discrete keys
  if (legend_is_continuous(x))
    return(color_nodes_legend_continuous(object))

  # Discrete attributes (factor/logical/small integer): categorical legend
  values <- attr(x, "cpal")

  print(nplot_legend(
    object,
    labels = names(values),
    pch    = 21,
    gp     = grid::gpar(fill = values)
  ))

}

#' Decide whether a color mapping should use a continuous (color bar) legend
#'
#' Factors and logicals are always discrete. Numeric attributes are continuous.
#' Integer attributes are treated as continuous only when they take many
#' distinct values (otherwise a categorical legend is clearer).
#' @param x A `netplot_color_nodes` object.
#' @noRd
legend_is_continuous <- function(x) {

  attr_type <- attr(x, "attr_type")

  if (attr_type == "numeric")
    return(TRUE)

  if (attr_type == "integer")
    return(length(unique(attr(x, "value"))) > 15L)

  FALSE

}

#' Draw a `netplot` object together with a continuous color-bar legend
#'
#' Called by [print.netplot()] when `vertex.color` was mapped from a continuous
#' attribute. The network is drawn first and the color bar is overlaid in the
#' top-right corner of the device.
#' @param object A `netplot` object with a continuous `.legend_vertex_fill`.
#' @noRd
color_nodes_legend_continuous <- function(object) {

  x       <- object$.legend_vertex_fill
  values  <- attr(x, "value")
  palette <- attr(x, "palette")
  main    <- attr(x, "attr_name")

  rng  <- range(values, na.rm = TRUE)
  cols <- grDevices::colorRampPalette(palette)(100) # low -> high

  # Tick marks within the observed range
  ticks <- pretty(rng, n = 4L)
  ticks <- ticks[ticks >= rng[1] & ticks <= rng[2]]
  if (length(ticks) < 2L)
    ticks <- rng

  # Draw the network first (without recursing into the legend logic)
  print(object, legend = FALSE, newpage = TRUE)

  # Overlay the color bar in the top-right corner of the device
  vp <- grid::viewport(
    x      = grid::unit(1, "npc") - grid::unit(1.5, "lines"),
    y      = grid::unit(0.78, "npc"),
    width  = grid::unit(0.6, "lines"),
    height = grid::unit(0.30, "npc"),
    just   = c("right", "center"),
    yscale = rng,
    name   = "netplot-colorkey"
  )

  grid::pushViewport(vp)
  on.exit(grid::upViewport(), add = TRUE)

  # The gradient (top = high value)
  grid::grid.raster(
    grDevices::as.raster(matrix(rev(cols), ncol = 1L)),
    width       = grid::unit(1, "npc"),
    height      = grid::unit(1, "npc"),
    interpolate = TRUE
  )

  # A thin frame around the bar
  grid::grid.rect(gp = grid::gpar(fill = NA, col = "gray40", lwd = .5))

  # Tick labels to the right of the bar
  grid::grid.text(
    label = format(ticks, trim = TRUE),
    x     = grid::unit(1, "npc") + grid::unit(0.3, "lines"),
    y     = grid::unit(ticks, "native"),
    just  = "left",
    gp    = grid::gpar(fontsize = 8)
  )

  # Title above the bar
  if (length(main) && nzchar(main))
    grid::grid.text(
      label = main,
      x     = grid::unit(0.5, "npc"),
      y     = grid::unit(1, "npc") + grid::unit(0.7, "lines"),
      just  = "bottom",
      gp    = grid::gpar(fontsize = 9, fontface = "bold")
    )

  invisible(object)

}


if (FALSE) {

  # Factor attribute
  g1 <- graph_from_data_frame(d = data.frame(from = c("1", "2", "3"),
                                            to = c("2", "3", "1")),
                              directed = FALSE)

  # Assign unique vertex names
  set_vertex_attr(g1, "name", value = c("v1", "v2", "v3"))

  # Add group attribute to graph as a vertex attribute
  vertex_attr(g1)$group <- c("group1", "group2", "group3")

  # Color nodes by group attribute
  vcolors <- color_nodes(g1, "group")

  # nplot(..., vertex.color = vcolors)
  attr(vcolors, "map")

  color_nodes(g1 ~ group)

  # Here is an example using color_nodes with
  # netplot
  nplot(g1, vertex.color = color_nodes(g1, "group"))
  nplot(
    g1,
    vertex.color = color_nodes(g1, "group"),
    edge.color = "black"
    )

  nplot(
    g1,
    vertex.color = color_nodes(g1, "group"),
    edge.color = "black",
    # The V() function is to access the vertices in
    # igraph. Vertex attributes can then be accessed
    # using the $ operator
    vertex.label = V(g1)$name
    )

  nplot(
    g1,
    vertex.color = color_nodes(g1, "group"),
    edge.color = "black",
    vertex.label = c("A", "B", "C")
    )


  # Numeric attribute
  g2 <- graph_from_data_frame(d = data.frame(from = c(1, 2, 3),
                                            to = c(2, 3, 1)),
                              directed = FALSE)
  V(g2)$value <- c(1, 3, 2)
  color_nodes(g2, "value", "Blues")



  # Logical attribute
  g3 <- graph_from_data_frame(d = data.frame(from = c(1, 2, 3),
                                            to = c(2, 3, 1)),
                              directed = FALSE)
  V(g3)$selected <- c(TRUE, FALSE, TRUE)
  color_nodes(g3, "selected")



  # Invalid palette name
  g4 <- graph_from_data_frame(d = data.frame(from = c(1, 2, 3),
                                            to = c(2, 3, 1)),
                              directed = FALSE)
  V(g4)$group <- factor(c("A", "A", "B"))
  color_nodes(g4, "group", "InvalidPalette")



  # Attribute that does not exist
  color_nodes(g1, "fake_attr")

}
