#' Extract a graph attribute
#' @param graph A graph object of class igraph or network.
#' @param attribute A character string specifying the name of the attribute.
#' @return A vector of the attribute values. If the attribute does not exist, an error is thrown.
#' @noRd
get_graph_attribute <- function(graph, attribute) UseMethod("get_graph_attribute")

get_graph_attribute.igraph <- function(graph, attribute) {

  # Check if the attribute exists
  if (!(attribute %in% igraph::vertex_attr_names(graph))) {
    stop("Attribute does not exist in graph")
  }

  # Extract the attribute
  igraph::vertex_attr(graph, name = attribute)

}

get_graph_attribute.network <- function(graph, attribute) {

  # Check if the attribute exists
  if (!(attribute %in% network::list.vertex.attributes(graph))) {
    stop("Attribute does not exist in graph")
  }

  # Extract the attribute
  network::get.vertex.attribute(graph, attribute)

}

get_graph_attribute.default <- function(graph, attribute) {

  stop("Graph type not supported")

}

color_nodes <- function(...) UseMethod("color_nodes")

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

color_nodes.default <- function(
  graph,
  attribute,
  palette = grDevices::palette(),
  na_color = "white",
  ...
  ) {

  # Extracting the attribute from the graph
  value <- get_graph_attribute(graph, attribute)
  attr_type <- class(value)

  # Identifying NAs
  na_idx <- which(is.na(value))

  # Handle characters, are turned into factors
  if (attr_type == "character") {

    value <- as.factor(value)
    attr_type <- "factor"

  }

  # Handle factors
  if (attr_type == "factor") {

    # Map levels to colors
    cpal  <- grDevices::colorRampPalette(palette)(nlevels(value))

    # Creating mapping to recover colors
    names(cpal) <- levels(value)

    value <- cpal[as.integer(value)]

  }  else if (attr_type == "numeric") { # Handle numerics

    # Find min and max
    attr_min <- min(value)
    attr_max <- max(value)

    # Create color scale
    value <- grDevices::colorRamp(palette)(
      (attr_min:attr_max - attr_min)/(attr_max - attr_min)
    )

    cpal <- grDevices::rgb(
      grDevices::colorRamp(palette)(c(0, 1)),
      maxColorValue = 255
    )

    names(cpal) <- c(attr_min, attr_max)

    # Color nodes based on attribute value
    value <- grDevices::rgb(value, maxColorValue = 255)

  } else if (attr_type == "logical") { # Handle logicals


    # Creating mapping to recover colors
    cpal <- palette[1:2]
    names(cpal) <- c("FALSE", "TRUE")

    # Color nodes
    value <- cpal[as.integer(value) + 1]
  }

  # Handle other types (characters, dates)
  else {
    stop("Attribute type not supported")
  }

  value[na_idx] <- na_color

  structure(
    value,
    class = "netplot_color_nodes",
    attr_type = attr_type,
    palette = palette,
    na_color = na_color,
    map      = cpal
  )

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
