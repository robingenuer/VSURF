context("Global VSURF test for regression Orange data")

set.seed(2219, kind = "Mersenne-Twister")
data(Orange)
Orange[, 4:10] <- rnorm(7*nrow(Orange))
orange.vsurf <- VSURF(circumference~., Orange,
                      ntree.thres = 100, ntree.interp = 500, ntree.pred = 500,
                      nfor.thres = 20, nfor.interp = 10, nfor.pred = 10,
                      verbose = FALSE)

test_that("Selected variables for the 3 steps", {
  # skip_on_os(os = "mac", arch = "aarch64")
  expect_identical(orange.vsurf$varselect.thres, c(2L, 1L, 5L))
  expect_identical(orange.vsurf$varselect.interp, c(2L, 1L))
  expect_identical(orange.vsurf$varselect.pred, 2L)
})

test_that("Variable importance",{
  # skip_on_os(os = "mac", arch = "aarch64")
  if (substr(sessionInfo()$platform, 1, 22) == "aarch64-apple-darwin25") {
    expect_equal(orange.vsurf$imp.mean.dec,
                 c(2956.4, 201.3, 101.2, 13.9, -13.2,
                   -29.6, -29.9, -36.7, -37.8),
                 tolerance = 1e-1)
    expect_equal(orange.vsurf$imp.sd.dec,
                 c(266.5, 51.2, 98.2, 52.6, 61.6, 46.8,
                   57.5, 39.1, 41.2),
                 tolerance = 1e-1)
    expect_identical(orange.vsurf$imp.mean.dec.ind,
                     c(2L, 1L, 5L, 8L, 6L, 9L, 7L, 3L, 4L))
  } else {
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
  }
})

test_that("OOB erros of nested models", {
  # skip_on_os(os = "mac", arch = "aarch64")
  if (substr(sessionInfo()$platform, 1, 22) == "aarch64-apple-darwin25") {
    expect_equal(orange.vsurf$err.interp,
                 c(723, 447, 774),
                 tolerance = 1e-1)
    expect_equal(orange.vsurf$err.pred, 721, tolerance = 1e-1)
  } else {
    expect_equal(orange.vsurf$err.interp,
                 c(723.4782, 447.9998, 779.2947),
                 tolerance = 1e-4)
    expect_equal(orange.vsurf$err.pred, 721.4986, tolerance = 1e-4)
  }
})

test_that("Thresholds for the 3 steps", {
  # skip_on_os(os = "mac", arch = "aarch64")
  if (substr(sessionInfo()$platform, 1, 22) == "aarch64-apple-darwin25") {
    expect_equal(min(orange.vsurf$pred.pruned.tree), 56,
                 tolerance = 1e-1)
    expect_equal(orange.vsurf$sd.min, 19, tolerance = 1e-1)
    expect_equal(orange.vsurf$mean.jump, 327, tolerance = 1e-1)
  } else {
    expect_equal(min(orange.vsurf$pred.pruned.tree), 42.413,
                 tolerance = 1e-3)
    expect_equal(orange.vsurf$sd.min, 16.85953, tolerance = 1e-5)
    expect_equal(orange.vsurf$mean.jump, 331.2949, tolerance = 1e-4)
  }
})
