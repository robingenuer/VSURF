context("Global VSURF test for regression Orange data")

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
                 c(3066.1367959, 192.6666525, 118.7935090, 11.6102848, 0.9062662,
                   -22.4358139, -30.1065224, -55.3442207, -58.5611105),
               tolerance = 1e-7)
  expect_equal(orange.vsurf$imp.sd.dec,
                 c(220.84723, 58.58310, 48.57572, 50.87805, 72.14951, 66.42639,
                   75.21486, 41.26995, 42.87575),
               tolerance = 1e-5)
  expect_identical(orange.vsurf$imp.mean.dec.ind,
                     c(2L, 1L, 5L, 8L, 6L, 3L, 9L, 7L, 4L))
})

test_that("OOB erros of nested models", {
  expect_equal(orange.vsurf$err.interp,
               c(719.6904, 464.9525, 784.7824),
               tolerance = 1e-4)
  expect_equal(orange.vsurf$err.pred, 724.8328, tolerance = 1e-4)
})

test_that("Thresholds for the 3 steps", {
  expect_equal(min(orange.vsurf$pred.pruned.tree), 42.07285,
               tolerance = 1e-5)
  expect_equal(orange.vsurf$sd.min, 28.52897, tolerance = 1e-5)
  expect_equal(orange.vsurf$mean.jump, 319.8299, tolerance = 1e-4)
})
