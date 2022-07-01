context("(skip on CRAN for windows 32bit) Global VSURF test for classification iris data")

platform <- sessionInfo()$platform
is.win32b <- function(platform) {
  if (platform == "i386-w64-mingw32/i386 (32-bit)") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

set.seed(2219, kind = "Mersenne-Twister")
data(iris)
iris.vsurf <- VSURF(iris[,1:4], iris[,5],
                    ntree.thres = 100, ntree.interp = 500, ntree.pred = 500,
                    nfor.thres = 20, nfor.interp = 10, nfor.pred = 10, verbose = FALSE)

test_that("Selected variables for the 3 steps", {
  if (is.win32b(platform)) {
    skip_on_cran()
    skip_on_appveyor()}
  expect_identical(iris.vsurf$varselect.thres, c(4L, 3L, 1L, 2L))
  expect_identical(iris.vsurf$varselect.interp, c(4L, 3L))
  expect_identical(iris.vsurf$varselect.pred, c(4L, 3L))
})

test_that("Variable importance",{
  if (is.win32b(platform)) {
    skip_on_cran()
    skip_on_appveyor()}
  expect_equal(iris.vsurf$imp.mean.dec,
               c(0.26514650, 0.26355895, 0.08523059, 0.03936667),
               tolerance = 1e-7)
  expect_equal(iris.vsurf$imp.sd.dec,
               c(0.014059314, 0.013751759, 0.009897334, 0.006062447),
               tolerance = 1e-7)
  expect_identical(iris.vsurf$imp.mean.dec.ind, c(4L, 3L, 1L, 2L))
})

test_that("OOB erros of nested models", {
  if (is.win32b(platform)) {
    skip_on_cran()
    skip_on_appveyor()}
  expect_equal(iris.vsurf$err.interp,
               c(0.04666667, 0.03866667, 0.05000000, 0.04466667),
               tolerance = 1e-7)
  expect_equal(iris.vsurf$err.pred,
               c(0.04666667, 0.03600000),
               tolerance = 1e-7)
})

test_that("Thresholds for the 3 steps", {
  if (is.win32b(platform)) {
    skip_on_cran()
    skip_on_appveyor()}
  expect_equal(min(iris.vsurf$pred.pruned.tree), 0.006062447,
               tolerance = 1e-7)
  expect_equal(iris.vsurf$sd.min, 0.005258738,
               tolerance = 1e-7)
  expect_equal(iris.vsurf$mean.jump, 0.008333333, tolerance = 1e-7)
})