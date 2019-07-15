
<!-- README.md is generated from README.Rmd. Please edit that file -->
VSURF
=====

[![Travis-CI Build Status](https://travis-ci.org/robingenuer/VSURF.svg?branch=master)](https://travis-ci.org/robingenuer/VSURF) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/robingenuer/VSURF?branch=master&svg=true)](https://ci.appveyor.com/project/robingenuer/VSURF) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/VSURF)](https://cran.r-project.org/package=VSURF) [![CRAN Downloads month](https://cranlogs.r-pkg.org/badges/VSURF?color=blue)](https://www.r-pkg.org/pkg/VSURF) [![CRAN Downloads overall](https://cranlogs.r-pkg.org/badges/grand-total/VSURF?color=blue)](https://www.r-pkg.org/pkg/VSURF)

Variable Selection Using Random Forests

R package, implementing a three steps variable selection procedure based on random forests. Initially developed to handle high dimensional data (for which number of variables largely exceeds number of observations), the package is very versatile and can treat most dimensions of data, for regression and supervised classification problems. First step is dedicated to eliminate irrelevant variables from the dataset. Second step aims to select all variables related to the response for interpretation purpose. Third step refines the selection by eliminating redundancy in the set of variables selected by the second step, for prediction purpose.

Install
-------

To install the latest released version available on CRAN, use:

``` r
install.packages("VSURF")
```

To try the current development version from github, use:

``` r
if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")}
remotes::install_github("robingenuer/VSURF")
```
