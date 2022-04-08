# incidentally <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version/incidentally?color=orange)](https://cran.r-project.org/package=incidentally)
[![](http://cranlogs.r-pkg.org/badges/grand-total/incidentally?color=blue)](https://cran.r-project.org/package=incidentally)
[![](http://cranlogs.r-pkg.org/badges/last-month/incidentally?color=green)](https://cran.r-project.org/package=incidentally)
<!-- badges: end -->

## Welcome
Welcome to the `incidentally` package\! The incidentally package implements methods to generate incidence matrices, which can represent bipartite (aka two-mode, affiliation) networks. It is currently *under development* but the existing functions should mostly work.

## Installation
The /release branch contains the current CRAN release of the incidentally package. You can install it from [CRAN](https://CRAN.R-project.org) with:
``` r
install.packages("incidentally")
```

The /devel branch contains the working beta version of the next release of the incidentally package. All the functions are documented and have undergone various levels of preliminary debugging, so they should mostly work, but there are no guarantees. Feel free to use the devel version (with caution), and let us know if you run into any problems. You can install it You can install from GitHub with:
``` r
library(devtools)
install_github("zpneal/incidentally", ref = "devel", build_vignettes = TRUE)
```
