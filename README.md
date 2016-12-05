# ggparallel package
Heike Hofmann, Marie Vendettuoli  
December 2, 2016  



[![CRAN Status](http://www.r-pkg.org/badges/version/ggparallel)](http://cran.r-project.org/package=ggparallel)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/ggparallel)](http://www.r-pkg.org/pkg/ggparallel)
[![Travis-CI Build Status](https://travis-ci.org/heike/ggparallel.svg?branch=master)](https://travis-ci.org/heike/ggparallel)

ggparallel implements and combines different types of parallel coordinate plots for categorical data: hammock plots, parallel sets plots, common angle plots, and common angle plots with a hammock-like adjustment for line widths.


```r
library(ggparallel)
data(mtcars)

ggparallel(list("gear", "cyl"), data=mtcars)
```

![](README_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
ggparallel(list("gear", "cyl"), data=mtcars, method="hammock", ratio=0.25)
```

![](README_files/figure-html/unnamed-chunk-1-2.png)<!-- -->
