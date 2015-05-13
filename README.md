
# VSURF [![Travis-CI Build Status](https://travis-ci.org/robingenuer/VSURF.png?branch=master)](https://travis-ci.org/robingenuer/VSURF)

Variable Selection Using Random Forests

R package, implementing a three steps variable selection procedure based on random forests.
Initially developed to handle high dimensional data (for which number of
variables largely exceeds number of observations), the package is very
versatile and can treat most dimensions of data, for regression and
supervised classification problems. First step is dedicated to eliminate
irrelevant variables from the dataset. Second step aims to select all
variables related to the response for interpretation purpose. Third step
refines the selection by eliminating redundancy in the set of variables
selected by the second step, for prediction purpose.

## Install

To install the latest released version available on CRAN, use :


```r
install.packages("VSURF")
```

To try the current development version from github, use :


```r
devtools::install_github("robingenuer/VSURF")
# This needs the devtools package to be installed :
# install.packages("devtools")
```

