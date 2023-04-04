
#' For internal use
#' Manages naming convention for netplot grobs
#' @noRd
netplot_name <- (function() {
  # grid::gPath("graph", netplot_name$make(idx[[i]]), element)
  # Counter
  index <- 0L

  list(
    # Makes the name of a grob (edge/vertex/graph)
    make = function(idx, for_graph = FALSE) {

      if (for_graph)
        sprintf("graph.%i", idx[1])
      else if (length(idx) == 2)
        sprintf("edge.%i-%i", idx[1], idx[2])
      else if (length(idx) == 1L)
        sprintf("vertex.%i", idx)
      else
        stop("Invalid type.", call. = FALSE)

    },
    # Get the path
    path = function(idx, element = NULL, graph = NULL) {

      # # if no graph, by default the last one
      # if (!length(graph))
      #   graph <- netplot_name$last_graph()

      # Getting type
      type <- switch (length(idx),
        `1` = "vertex",
        `2` = "edge",
        stop("Incorrect number of indices in `idx`.")
      )

      # Validating elements
      np_validate$elements(element, type)

      do.call(
        grid::gPath,
        list(
          "graph", netplot_name$make(idx),
          element
          )
        )
    },
    # Creates a new graph increasing index to 1
    new = function() {

      index <<- index + 1L
      sprintf("graph.%i", index)

    },
    # Returns the last graph number
    last_graph = function() {
      netplot_name$make(index, for_graph = TRUE)
    }
  )

})()


# GPAR
.grid_par_names <- names(grid::get.gpar())
graphics.off() # To make sure it is closed after the call


#' Validating element types
#' @noRd
np_validate <- (function() {

  # Valid elements
  vertex <- c("core", "frame", "label")
  edge   <- c("arrow", "line")

  list(
    # Class
    is_netplot = function(x) {

      if (!inherits(x, "netplot"))
        stop("This function is only applicable for 'netplot' class objects.",
             call. = FALSE)

      invisible()

    },
    # Elements
    elements = function(elements, type) {

      # Is there anything to revise?
      if (missing(elements) || is.null(elements))
        return(invisible())

      test <- switch (
        type,
        vertex = which(!(elements %in% vertex)),
        edge   = which(!(elements %in% edge)),
        stop("The `type` should be either 'vertex' or 'edge'.", call. = FALSE)
      )

      if (length(test))
        stop("Invalid element type. The following `element`(s) for ",
             type, " are wrongly specified: '",
             paste(elements, collapse="', '"), "'. ", call.=FALSE)

      invisible()

    },
    #Type
    type = function(type, multiple = FALSE) {

      # Missing
      if (missing(type))
        stop("Unspecified (missing) `type`. You must provide either {'vertex', 'edge'}",
             call. = FALSE)

      # More than one?
      if (!multiple && length(type) != 1)
        stop("Only one type, vertex or edge, can be specified.", call. = FALSE)

      test <- which(!(type %in% c("vertex", "edge")))
      if (length(test))
        stop(
          "Invalid `type` ('", paste0(type[test], collapse="', '"),"'. ",
          "`type` should be one of {'vertex', 'edge'}."
          )

      invisible()


    },
    gpar = function(gpar) {

      if (is.list(gpar))
        gpar <- names(gpar)

      # Are the par aesthetics registered?
      test <- which(!(gpar %in% .grid_par_names))
      if (length(test))
        stop("The following parameters are not part of `grid::gpar`: '",
             paste(gpar[test], collapse="', '"), "'.",
             "Supported parameters are: '",
             paste(.grid_par_names, collapse="', '"),"'.",
             call. = FALSE)

      invisible()
    }
  )

})()
