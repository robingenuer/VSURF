context("Global VSURF test for classification iris data")

set.seed(2219, kind = "Mersenne-Twister")
data(iris)
iris.vsurf <- VSURF(iris[,1:4], iris[,5], ntree = 100, nfor.thres = 20,
                    nfor.interp = 10, nfor.pred = 10)

test_that("Selected variables for the 3 steps", {
  expect_identical(iris.vsurf$varselect.thres, c(4L, 3L, 1L, 2L))
  expect_identical(iris.vsurf$varselect.interp, c(4L, 3L))
  expect_identical(iris.vsurf$varselect.pred, c(4L, 3L))
})

test_that("Variable importance",{
  expect_equal(iris.vsurf$imp.mean.dec,
               c(0.26633637, 0.25610509, 0.09020064, 0.03915156),
               tolerance = 1e-7)
  expect_equal(iris.vsurf$imp.sd.dec,
               c(0.021659115, 0.015990696, 0.012599931, 0.007075411),
               tolerance = 1e-7)
  expect_identical(iris.vsurf$imp.mean.dec.ind, c(4L, 3L, 1L, 2L))
})

test_that("OOB erros of nested models", {
  expect_equal(iris.vsurf$err.interp,
               c(0.04666667, 0.03600000, 0.05000000, 0.04533333),
               tolerance = 1e-7)
  expect_equal(iris.vsurf$err.pred,
               c(0.04666667, 0.03466667),
               tolerance = 1e-7)
})

test_that("Thresholds for the 3 steps", {
  expect_equal(min(iris.vsurf$pred.pruned.tree), 0.007075411,
               tolerance = 1e-7)
  expect_equal(iris.vsurf$sd.min, 0.003442652,
               tolerance = 1e-7)
  expect_equal(iris.vsurf$mean.jump, 0.009333333, tolerance = 1e-7)
})