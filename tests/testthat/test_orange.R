context("Global VSURF test for regression Orange data")

platform <- sessionInfo()$platform
is.win32b <- function(platform) {
  if (platform == "i386-w64-mingw32/i386 (32-bit)") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

set.seed(2219, kind = "Mersenne-Twister")
data(Orange)
Orange[, 4:10] <- rnorm(7*nrow(Orange))
orange.vsurf <- VSURF(circumference~., Orange,
                      ntree.thres = 100,ntree.interp = 500, ntree.pred = 500,
                      nfor.thres = 20, nfor.interp = 10, nfor.pred = 10, verbose = FALSE)

test_that("Selected variables for the 3 steps", {
  if (is.win32b(platform)) {
    skip_on_cran()
    skip_on_appveyor()}
  expect_identical(orange.vsurf$varselect.thres, c(2L, 1L, 5L))
  expect_identical(orange.vsurf$varselect.interp, c(2L, 1L))
  expect_identical(orange.vsurf$varselect.pred, 2L)
})

test_that("Variable importance",{
  if (is.win32b(platform)) {
    skip_on_cran()
    skip_on_appveyor()}
  expect_equal(orange.vsurf$imp.mean.dec,
                 c(2954.46142, 216.26601, 75.09309, 19.92371, -24.72205,
                   -31.97516, -39.00297, -40.30447, -54.48215),
               tolerance = 1e-5)
  expect_equal(orange.vsurf$imp.sd.dec,
                 c(200.81689, 59.45693, 65.94583, 55.68584, 58.02011, 44.16739,
                   57.14253, 46.91170, 42.41300),
               tolerance = 1e-5)
  expect_identical(orange.vsurf$imp.mean.dec.ind,
                     c(2L, 1L, 5L, 8L, 7L, 3L, 6L, 9L, 4L))
})

test_that("OOB erros of nested models", {
  if (is.win32b(platform)) {
    skip_on_cran()
    skip_on_appveyor()}
  expect_equal(orange.vsurf$err.interp,
               c(723.4782, 447.9998, 779.2947),
               tolerance = 1e-4)
  expect_equal(orange.vsurf$err.pred, 721.4986, tolerance = 1e-4)
})

test_that("Thresholds for the 3 steps", {
  if (is.win32b(platform)) {
    skip_on_cran()
    skip_on_appveyor()}
  expect_equal(min(orange.vsurf$pred.pruned.tree), 42.413,
               tolerance = 1e-3)
  expect_equal(orange.vsurf$sd.min, 16.85953, tolerance = 1e-5)
  expect_equal(orange.vsurf$mean.jump, 331.2949, tolerance = 1e-4)
})
