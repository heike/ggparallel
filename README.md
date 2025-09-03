
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggparallel

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ggparallel)](https://CRAN.R-project.org/package=ggparallel)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/last-month/ggparallel?color=blue)](https://r-pkg.org/pkg/ggparallel)
[![Last-changedate](https://img.shields.io/badge/last%20change-2025--09--03-yellowgreen.svg)](https://github.com/heike/ggparallel/commits/main)
[![codecov test
coverage](https://codecov.io/gh/heike/ggparallel/graph/badge.svg?token=zfeqffIjxY)](https://codecov.io/gh/heike/ggparallel)
[![R-CMD-check](https://github.com/heike/ggparallel/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/heike/ggparallel/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The R package `ggparallel` implements and combines different types of
parallel coordinate plots for categorical data: [hammock
plots](http://www.schonlau.net/publication/03jsm_hammockplot_old.pdf),
[parallel sets
plots](https://datavizcatalogue.com/methods/parallel_sets.html), and
[common angle plots](https://ieeexplore.ieee.org/document/6634157), as
well as common angle plots with a hammock-like adjustment for line
widths.

## Installation

The package is available on CRAN:

    install.packages("ggparallel")

You can install the development version of ggparallel from
[GitHub](https://github.com/) with:

    # install.packages("remotes")
    remotes::install_github("heike/ggparallel")

## Basic use case

``` r
library(ggparallel)
#> Loading required package: ggplot2
data(mtcars)

ggparallel(list("gear", "cyl"), data=mtcars)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r
ggparallel(list("gear", "cyl"), data=mtcars, method="hammock", ratio=0.25)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />
