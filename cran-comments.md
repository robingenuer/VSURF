## Test environments
* local ubuntu 22.04.5 install, R 4.5.1
* github actions (mac, windows, ubuntu release/devel/oldrel-1)
* rhub (m1-san, macos, macos-arm64)

## R CMD check results
There were no WARNINGs, ERRORS nor NOTEs.

I try all mac configs that I can access and all tests passed. Since I still have
differences with the M1mac config on CRAN submission (I do not know how to
access such a config to test beforehand) maybe because of the set.seed()
different behavior on this config, I propose to add a case in my tests
files. I think it is not an ideal solution but I would be happy to know another
way.

## Downstream dependencies
* MSclassifR
* SAiVE