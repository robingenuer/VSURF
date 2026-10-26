## Test environments
* local ubuntu 26.04.1 install, R 4.6.1
* github actions (macos release, windows release, ubuntu release/devel/oldrel-1)
* rhub (linux, m1-san, macos, macos-arm64, windows)
* devtools::check_win_devel()

## R CMD check results
There were no WARNINGs, ERRORS nor NOTEs.

Maybe because of the set.seed() different behavior on mac aarch64-apple-darwin,
I update this case in my tests files. All platforms seems to have the same
behavior, so all tests are now passing.

## Downstream dependencies
* MSclassifR
* SAiVE