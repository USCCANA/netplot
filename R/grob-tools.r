
#' For internal use
#' Manages naming convention for netplot grobs
#' @noRd
netplot_name <- (function() {

  # Counter
  index <- 0L

  list(
    # Makes the name of a grob (edge/vertex/graph)
    make = function(idx, for_graph = FALSE) {

      if (for_graph)
        paste0("graph.", idx[1])
      else if (length(idx) == 2)
        sprintf("edge.%i-%i", idx[1], idx[2])
      else if (length(idx) == 1L)
        paste0("vertex.", idx)
      else
        stop("Invalid type.", call. = FALSE)

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


#' Validating element types
#' @noRd
netplot_elements <- (function() {

  vertex <- c("core", "frame", "label")
  edge   <- c("arrow", "line")

  list(
    validate = function(elements, type) {

      test <- switch (
        type,
        vertex = which(!(elements %in% vertex)),
        edge = which(!(elements %in% edge)),
        stop("The `type` should be either 'vertex' or 'edge'.", call. = FALSE)
      )

      if (length(test))
        stop("Invalid element type. The following `element`(s) for ",
             type, " are wrongly specified: '",
             paste(elements, collapse="', '"), "'. ", call.=FALSE)

      invisible()

    }
  )

})()
