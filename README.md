[![Travis-CI Build
Status](https://travis-ci.org/heike/ggparallel.svg?branch=master)](https://travis-ci.org/heike/ggparallel)

ggparallel implements and combines different types of parallel
coordinate plots for categorical data: hammock plots, parallel sets
plots, common angle plots, and common angle plots with a hammock-like
adjustment for line widths.

    library(ggparallel)
    data(mtcars)

    ggparallel(list("gear", "cyl"), data=mtcars)

![](README_files/figure-markdown_strict/unnamed-chunk-1-1.png)

    ggparallel(list("gear", "cyl"), data=mtcars, method="hammock", ratio=0.25)

![](README_files/figure-markdown_strict/unnamed-chunk-1-2.png)
