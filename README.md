
<!-- README.md is generated from README.Rmd. Please edit that file -->
netplot
=======

The goal of netplot is to provide a more flexible yet nicer graph plotting function with better defaults

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

plot(x) # ala igraph
```

<img src="man/figures/README-example-1.png" width="70%" />

``` r
nplot(x) # ala netplot
```

<img src="man/figures/README-example-2.png" width="70%" />
