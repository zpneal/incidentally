# incidentally <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version/incidentally?color=orange)](https://cran.r-project.org/package=incidentally)
[![](http://cranlogs.r-pkg.org/badges/grand-total/incidentally?color=blue)](https://cran.r-project.org/package=incidentally)
[![](http://cranlogs.r-pkg.org/badges/last-month/incidentally?color=green)](https://cran.r-project.org/package=incidentally)
[![status](https://tinyverse.netlify.com/badge/incidentally)](https://CRAN.R-project.org/package=incidentally)
[![DOI:10.31219/osf.io/ectms](http://img.shields.io/badge/DOI-10.31219/osf.io/ectms-B31B1B.svg)](https://doi.org/10.31219/osf.io/ectms)
<!-- badges: end -->

## Welcome
Welcome to the `incidentally` package\! The incidentally package implements methods to generate incidence matrices, which can represent bipartite (aka two-mode, affiliation) networks.

The `incidentally` package can be cited as:

**Neal, Z. P. (2022). incidentally: An R package for generating incidence matrices and bipartite graphs. *CRAN*. [https://doi.org/10.32614/CRAN.package.incidentally](https://doi.org/10.32614/CRAN.package.incidentally)**

Two extended functions in the `incidentally` package have their own citations, which provide extended tutorials. The `incidence.from.congress()` function can be cited as:

**Neal, Z. P. (2022). Constructing legislative networks in R using incidentally and backbone. *Connections, 42*, 1-9. [https://doi.org/10.2478/connections-2019.026](https://doi.org/10.2478/connections-2019.026)**

The `incidence.from.adjacency()` function can be cited as:

**Neal, Z. P. (2023). The duality of networks and groups: Models to generate two-mode networks from one-mode networks. *Network Science, 11*, 397-410. [https://doi.org/10.1017/nws.2023.3](https://doi.org/10.1017/nws.2023.3).**

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

## Dependencies
The `incidentally` package adopts the [tinyverse](https://www.tinyverse.org/) philosophy, and therefore aims to keep dependencies at a minimum.
