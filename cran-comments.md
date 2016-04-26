## Test environments
* local ubuntu 15.10 install, R 3.2.4
* linux (travis-ci), current devel, release and oldrel
* win-builder (devel and release)

## R CMD check results
There were no WARNINGs or NOTEs.
However their are some ERRORs for the tests with winbuilder
for windows 32-bit.
Those errors come from the fact that a run of randomForest()
is different for this platform.
What we check is the good behavior of our package VSURF and not
randomForest, and our code is OK (at least for our tests).
All CRAN package check results would be OK.

## Downstream dependencies
There are currently no downstream dependencies for this package.