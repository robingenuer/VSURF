context("set.seed test")

set.seed(2219, kind = "Mersenne-Twister")
def <- runif(1)

test_that("Different kind in set.seed", {
  expect_equal(def, 0.4305179, tolerance = 1e-7)
})
