context("Global VSURF parallel test for classification iris data")

if (!substr(sessionInfo()$platform, 1, 18) == "x86_64-w64-mingw32") {
# if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  set.seed(221921186, "L'Ecuyer-CMRG")
  data(iris)
  iris.vsurf <- VSURF(iris[,1:4], iris[,5],
                      ntree.thres = 100, ntree.interp = 500, ntree.pred = 500,
                      nfor.thres = 20,
                      nfor.interp = 10, nfor.pred = 10,
                      parallel = TRUE, ncores = 2, clusterType = "FORK",
                      verbose = FALSE)
}

test_that("Selected variables for the 3 steps", {
  skip_on_os(os = "windows")
  expect_identical(iris.vsurf$varselect.thres, c(3L, 4L, 1L, 2L))
  expect_identical(iris.vsurf$varselect.interp, c(3L, 4L))
  expect_identical(iris.vsurf$varselect.pred, c(3L, 4L))
})

test_that("Variable importance",{
  skip_on_os(os = "windows")
  expect_equal(iris.vsurf$imp.mean.dec,
               c(0.26655183, 0.26225915, 0.08670964, 0.03800202),
               tolerance = 1e-7)
  expect_equal(iris.vsurf$imp.sd.dec,
               c(0.013238650, 0.020562992, 0.006816689, 0.005869418),
               tolerance = 1e-7)
  expect_identical(iris.vsurf$imp.mean.dec.ind, c(3L, 4L, 1L, 2L))
})

test_that("OOB erros of nested models", {
  skip_on_os(os = "windows")
  expect_equal(iris.vsurf$err.interp,
               c(0.07600000, 0.03666667, 0.04933333, 0.04400000),
               tolerance = 1e-7)
  expect_equal(iris.vsurf$err.pred,
               c(0.07600000, 0.03666667),
               tolerance = 1e-7)
})

test_that("Thresholds for the 3 steps", {
  skip_on_os(os = "windows")
  expect_equal(min(iris.vsurf$pred.pruned.tree), 0.006343053,
               tolerance = 1e-7)
  expect_equal(iris.vsurf$sd.min, 0.003513642, tolerance = 1e-7)
  expect_equal(iris.vsurf$mean.jump, 0.009, tolerance = 1e-7)
})