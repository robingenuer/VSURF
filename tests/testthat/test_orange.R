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
orange.vsurf <- VSURF(circumference~., Orange, ntree = 100, nfor.thres = 20,
                      nfor.interp = 10, nfor.pred = 10)

test_that("Selected variables for the 3 steps", {
  expect_identical(orange.vsurf$varselect.thres, c(2L, 1L, 5L))
  expect_identical(orange.vsurf$varselect.interp, c(2L, 1L))
  expect_identical(orange.vsurf$varselect.pred, 2L)
})

test_that("Variable importance",{
  expect_equal(orange.vsurf$imp.mean.dec,
               if (!is.win32b(platform))
                 c(3066.1367959, 192.6666525, 118.7935090, 11.6102848, 0.9062662,
                   -22.4358139, -30.1065224, -55.3442207, -58.5611105) else
                     c(3018.152231, 205.100336, 84.341730, 3.709502, -3.080991,
                       -22.976198, -24.663545, -35.288642, -46.880073),
               tolerance = 1e-7)
  expect_equal(orange.vsurf$imp.sd.dec,
               if (!is.win32b(platform))
                 c(220.84723, 58.58310, 48.57572, 50.87805, 72.14951, 66.42639,
                   75.21486, 41.26995, 42.87575) else
                     c(240.12941, 74.93350, 65.43007, 62.30966, 81.52400, 
                       54.21183, 52.33703, 55.34055, 76.15944),
               tolerance = 1e-5)
  expect_identical(orange.vsurf$imp.mean.dec.ind,
                   if (!is.win32b(platform))
                     c(2L, 1L, 5L, 8L, 6L, 3L, 9L, 7L, 4L) else
                       c(2L, 1L, 5L, 8L, 6L, 7L, 3L, 9L, 4L))
})

test_that("OOB erros of nested models", {
  expect_equal(orange.vsurf$err.interp,
               if (!is.win32b(platform))
                 c(719.6904, 464.9525, 784.7824) else
                   c(718.5050, 464.6179, 780.1318),
               tolerance = 1e-4)
  expect_equal(orange.vsurf$err.pred,
               ifelse(!is.win32b(platform), 724.8328, 724.8492), tolerance = 1e-4)
})

test_that("Thresholds for the 3 steps", {
  expect_equal(min(orange.vsurf$pred.pruned.tree),
               ifelse(!is.win32b(platform), 42.07285, 52.33703),
               tolerance = 1e-5)
  expect_equal(orange.vsurf$sd.min,
               ifelse(!is.win32b(platform), 28.52897, 31.55871), tolerance = 1e-5)
  expect_equal(orange.vsurf$mean.jump,
               ifelse(!is.win32b(platform), 319.8299, 315.5139), tolerance = 1e-4)
})
