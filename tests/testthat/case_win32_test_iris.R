context("Global VSURF test for classification iris data")

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
iris.vsurf <- VSURF(iris[,1:4], iris[,5], ntree = 100, nfor.thres = 20,
                    nfor.interp = 10, nfor.pred = 10)

test_that("Selected variables for the 3 steps", {
  expect_identical(iris.vsurf$varselect.thres,
                   if (!is.win32b(platform)) c(4L, 3L, 1L, 2L) else c(3L, 4L, 1L, 2L))
  expect_identical(iris.vsurf$varselect.interp,
                   if (!is.win32b(platform)) c(4L, 3L) else c(3L, 4L))
  expect_identical(iris.vsurf$varselect.pred,
                   if (!is.win32b(platform)) c(4L, 3L) else c(3L, 4L))
})

test_that("Variable importance",{
  expect_equal(iris.vsurf$imp.mean.dec,
               if (!is.win32b(platform)) 
               c(0.26633637, 0.25610509, 0.09020064, 0.03915156) else
               c(0.26570904, 0.25934408, 0.08724491, 0.04029597),
               tolerance = 1e-7)
  expect_equal(iris.vsurf$imp.sd.dec,
               if (!is.win32b(platform)) 
               c(0.021659115, 0.015990696, 0.012599931, 0.007075411) else
               c(0.022367073, 0.021800526, 0.008624224, 0.007335576),
               tolerance = 1e-7)
  expect_identical(iris.vsurf$imp.mean.dec.ind,
                   if (!is.win32b(platform)) c(4L, 3L, 1L, 2L) else c(3L, 4L, 1L, 2L))
})

test_that("OOB erros of nested models", {
  expect_equal(iris.vsurf$err.interp,
               if (!is.win32b(platform)) 
               c(0.04666667, 0.03600000, 0.05000000, 0.04533333) else
               c(0.06933333, 0.03666667, 0.04600000, 0.04666667),
               tolerance = 1e-7)
  expect_equal(iris.vsurf$err.pred,
               if (!is.win32b(platform))
               c(0.04666667, 0.03466667) else
               c(0.07133333, 0.03533333),
               tolerance = 1e-7)
})

test_that("Thresholds for the 3 steps", {
  expect_equal(min(iris.vsurf$pred.pruned.tree),
               ifelse(!is.win32b(platform), 0.007075411, 0.007335576),
               tolerance = 1e-7)
  expect_equal(iris.vsurf$sd.min, 
               ifelse(!is.win32b(platform), 0.003442652, 0.004714045),
               tolerance = 1e-7)
  expect_equal(iris.vsurf$mean.jump, 
               ifelse(!is.win32b(platform), 0.009333333, 0.005), tolerance = 1e-7)
})