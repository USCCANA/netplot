% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/netplot.R
\name{locate_vertex}
\alias{locate_vertex}
\title{Find a vertex in the current plot}
\usage{
locate_vertex(x = NULL)
}
\arguments{
\item{x}{An object of class \code{netplot}}
}
\value{
A list with the name of the vertex, the x and y coordinates and the
viewport where it is located.
}
\description{
This function is a wrapper of \code{\link[grid:grid.locator]{grid::grid.locator()}}, and provides a way to
find the coordinates of a vertex in the current plot. It is useful to
identify the vertex that is being clicked in a plot.
}
\details{
This function only works in interactive mode. Once it is called,
the user can click on a vertex in the plot. The function will return the
name of the vertex, the x and y coordinates and the viewport where it is
located. If \code{x} is not specified, the last plotted \code{netplot} object will be
used.
}
\examples{
library(igraph)
library(netplot)
set.seed(1)
x <- sample_smallworld(1, 200, 5, 0.03)

# Plotting
nplot(x)

# Clicking (only works in interactive mode)
if (interactive()) {
 res <- locate_vertex()
 print(res)
}

}
