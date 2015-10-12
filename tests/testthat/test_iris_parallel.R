context("(skip on cran) Global VSURF parallel test for classification iris data")

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  set.seed(221921186, "L'Ecuyer-CMRG")
  data(iris)
  iris.vsurf <- VSURF(iris[,1:4], iris[,5], ntree = 100, nfor.thres = 20,
                      nfor.interp = 10, nfor.pred = 10,
                    parallel = TRUE, ncores = 4, clusterType = "FORK")
}

test_that("Selected variables for the 3 steps", {
  skip_on_cran()
  expect_identical(iris.vsurf$varselect.thres, c(3L, 4L, 1L, 2L))
  expect_identical(iris.vsurf$varselect.interp, c(3L, 4L))
  expect_identical(iris.vsurf$varselect.pred, c(3L, 4L))
})

test_that("Variable importance",{
  skip_on_cran()
  expect_equal(iris.vsurf$imp.mean.dec,
               c(0.26685454, 0.26609110, 0.08489910, 0.03596449),
               tolerance = 1e-7)
  expect_equal(iris.vsurf$imp.sd.dec,
               c(0.019565165, 0.017272263, 0.011743964, 0.006985874),
               tolerance = 1e-7)
  expect_identical(iris.vsurf$imp.mean.dec.ind, c(3L, 4L, 1L, 2L))
})

test_that("OOB erros of nested models", {
  skip_on_cran()
  expect_equal(iris.vsurf$err.interp,
               c(0.07266667, 0.03666667, 0.04600000, 0.04733333),
               tolerance = 1e-7)
  expect_equal(iris.vsurf$err.pred,
               c(0.07133333, 0.03466667),
               tolerance = 1e-7)
})

test_that("Thresholds for the 3 steps", {
  skip_on_cran()
  expect_equal(min(iris.vsurf$pred.pruned.tree), 0.006985874,
               tolerance = 1e-7)
  expect_equal(iris.vsurf$sd.min, 0.004714045, tolerance = 1e-7)
  expect_equal(iris.vsurf$mean.jump, 0.005333333, tolerance = 1e-7)
})