context("Global test for VSURF function")

set.seed(221921186)
data(iris)
iris.vsurf <- VSURF(iris[,1:4], iris[,5], ntree = 100, nfor.thres = 20,
                    nfor.interp = 10, nfor.pred = 10)

test_that("Selected variables for the 3 steps", {
  expect_identical(iris.vsurf$varselect.thres, c(3L, 4L, 1L, 2L))
  expect_identical(iris.vsurf$varselect.interp, c(3L, 4L))
  expect_identical(iris.vsurf$varselect.pred, c(3L, 4L))
})

test_that("Variable importances thresholding selected",{
  expect_equal(iris.vsurf$imp.varselect.thres,
               c(0.26841716, 0.25882658, 0.08853212, 0.03913999),
               tolerance = 1e-7)
})
