context("Global VSURF test for classification iris data")

set.seed(221921186)
data(iris)
iris.vsurf <- VSURF(iris[,1:4], iris[,5], ntree = 100, nfor.thres = 20,
                    nfor.interp = 10, nfor.pred = 10)

test_that("Selected variables for the 3 steps", {
  expect_identical(iris.vsurf$varselect.thres, c(3L, 4L, 1L, 2L))
  expect_identical(iris.vsurf$varselect.interp, c(3L, 4L))
  expect_identical(iris.vsurf$varselect.pred, c(3L, 4L))
})

test_that("Variable importance",{
  expect_equal(iris.vsurf$imp.mean.dec,
               c(0.26841716, 0.25882658, 0.08853212, 0.03913999),
               tolerance = 1e-7)
  expect_equal(iris.vsurf$imp.sd.dec,
               c(0.016673720, 0.016680544, 0.012353992, 0.007608147),
               tolerance = 1e-7)
  expect_identical(iris.vsurf$imp.mean.dec.ind, c(3L, 4L, 1L, 2L))
})

test_that("OOB erros of nested models", {
  expect_equal(iris.vsurf$err.interp,
               c(0.06866667, 0.03533333, 0.04733333, 0.04266667),
               tolerance = 1e-7)
  expect_equal(iris.vsurf$err.pred,
               c(0.06733333, 0.03733333),
               tolerance = 1e-7)
})

test_that("Thresholds for the 3 steps", {
  expect_equal(min(iris.vsurf$pred.pruned.tree), 0.007608147,
               tolerance = 1e-7)
  expect_equal(iris.vsurf$sd.min, 0.003220306, tolerance = 1e-7)
  expect_equal(iris.vsurf$mean.jump, 0.008333333, tolerance = 1e-7)
})