
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![CRAN
status](https://www.r-pkg.org/badges/version/netplot)](https://cran.r-project.org/package=netplot)
[![CRAN](https://cranlogs.r-pkg.org/badges/netplot)](https://cran.r-project.org/package=netplot)
[![R](https://github.com/USCCANA/netplot/actions/workflows/ci.yml/badge.svg)](https://github.com/USCCANA/netplot/actions/workflows/ci.yml)
[![Build
status](https://ci.appveyor.com/api/projects/status/3k2m3oq6o99qcs0r?svg=true)](https://ci.appveyor.com/project/gvegayon/netplot)
[![USC’s Department of Preventive
Medicine](https://raw.githubusercontent.com/USCbiostats/badges/master/tommy-uscprevmed-badge.svg)](https://preventivemedicine.usc.edu)

# netplot

An alternative graph visualization tool that emphasizes aesthetics,
providing default parameters that deliver out-of-the-box lovely
visualizations.

Some features:

1.  Auto-scaling of vertices using sizes relative to the plotting
    device.
2.  Embedded edge color mixer.
3.  True curved edges drawing.
4.  User-defined edge curvature.
5.  Nicer vertex frame color.
6.  Better use of space-filling the plotting device.

The package uses the `grid` plotting system (just like `ggplot2`).

## Installation

You can install the released version of netplot from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("netplot")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("USCCANA/netplot")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(igraph)
#> 
#> Attaching package: 'igraph'
#> The following objects are masked from 'package:stats':
#> 
#>     decompose, spectrum
#> The following object is masked from 'package:base':
#> 
#>     union
library(netplot)
#> Loading required package: grid
#> 
#> Attaching package: 'netplot'
#> The following object is masked from 'package:igraph':
#> 
#>     ego
set.seed(1)
data("UKfaculty", package = "igraphdata")
l <- layout_with_fr(UKfaculty)

plot(UKfaculty, layout = l) # ala igraph
```

<img src="man/figures/README-example-1.png" width="85%" />

``` r
nplot(UKfaculty, layout = l) # ala netplot
```

<img src="man/figures/README-example-2.png" width="85%" />

``` r
sna::gplot(intergraph::asNetwork(UKfaculty), coord=l)
```

<img src="man/figures/README-example-3.png" width="85%" />

### UKfaculty

``` r
# Random names
set.seed(1)
nam <- sample(babynames::babynames$name, vcount(UKfaculty))

ans <- nplot(
  UKfaculty,
  layout                = l,
  vertex.color          = grDevices::hcl.colors(5, "Plasma")[V(UKfaculty)$Group + 1],
  vertex.label          = nam,
  vertex.size.range     = c(.01, .04, 4),
  vertex.label.col      =  "black",
  vertex.label.fontface = "bold",
  bg.col                = "transparent",
  vertex.label.show     = .5,
  vertex.label.range    = c(10, 25),
  edge.width.range      = c(1, 4, 5)
  )


# Plot it!
ans
```

<img src="man/figures/README-unnamed-chunk-1-1.png" width="85%" />

``` r

# With a legend
nplot_legend(
  ans, 
  labels = paste("Group", 1:4),
  pch    = c(21, 21, 21),
  gp     = gpar(fill = grDevices::hcl.colors(5, "Plasma")[2:4])
  )
```

<img src="man/figures/README-unnamed-chunk-1-2.png" width="85%" />

Starting version 0.2-0, we can use gradients!

``` r
ans |>
  set_vertex_gpar(
    element = "core",
    fill = lapply(get_vertex_gpar(ans, "frame", "col")$col, \(i) {
      radialGradient(c("white", i), cx1=.8, cy1=.8, r1=0)
      }))
```

<img src="man/figures/README-uk-faculty-gradient-1.png" width="85%" />

### USairports

``` r
# Loading the data
data(USairports, package="igraphdata")

# Generating a layout naively
layout   <- V(USairports)$Position
layout   <- do.call(rbind, lapply(layout, function(x) strsplit(x, " ")[[1]]))
layout[] <- stringr::str_remove(layout, "^[a-zA-Z]+")
layout   <- matrix(as.numeric(layout[]), ncol=2)

# Some missingness
layout[which(!complete.cases(layout)), ] <- apply(layout, 2, mean, na.rm=TRUE)

# Have to rotate it (it doesn't matter the origin)
layout <- netplot:::rotate(layout, c(0,0), pi/2)

# Simplifying the network
net <- simplify(USairports, edge.attr.comb = list(
  weight = "sum",
  name   = "concat",
  Passengers = "sum",
  "ignore"
))

# Pretty graph
nplot(
  net,
  layout            = layout,
  edge.width        = E(net)$Passengers,
  edge.color        = ~ ego(alpha = 0) + alter(col = "yellow", alpha = .75),
  skip.vertex       = TRUE,
  skip.arrows       = TRUE,
  edge.width.range  = c(.75, 4, 4), 
  bg.col            = "black",
  edge.line.breaks  = 10
  )
```

<img src="man/figures/README-us-airports-1.png" width="85%" />
