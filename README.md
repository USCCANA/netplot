
<!-- README.md is generated from README.Rmd. Please edit that file -->
netplot
=======

The goal of netplot is to provide a more flexible yet nicer graph plotting function with better defaults

Some features:

1.  Auto-scaling of vertices using sizes relative to the plotting device.
2.  Embedded edge color mixer.
3.  True curved edges drawing.
4.  User-defined edge curvature.
5.  Nicer vertex frame color.
6.  Better use of space filling the plotting device.

Installation
------------

<!-- You can install the released version of netplot from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("netplot") -->
<!-- ``` -->
And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("USCCANA/netplot")
```

Example
-------

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
set.seed(1)
x <- sample_smallworld(1, 200, 5, 0.03)
l <- layout_with_fr(x)

plot(x, layout = l) # ala igraph
```

<img src="man/figures/README-example-1.png" width="70%" />

``` r
nplot(x, layout = l) # ala netplot
```

<img src="man/figures/README-example-2.png" width="70%" />

``` r
sna::gplot(intergraph::asNetwork(x), coord=l)
```

<img src="man/figures/README-example-3.png" width="70%" />

``` r
nplot(
  x,
  edge.curvature = pi,
  edge.color.alpha = .2,
  edge.width.range = c(2,2),
  bg.col = "white",
  layout = l
  )
```

<img src="man/figures/README-unnamed-chunk-1-1.png" width="70%" />
