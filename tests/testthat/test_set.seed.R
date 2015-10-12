context("randomness in runif and randomForest")

set.seed(2219)
def <- runif(1)

test_that("Different kind in set.seed", {
  expect_equal(def, 0.4305179, tolerance = 1e-7)
})

data(iris)
iris.rf <- randomForest(iris[,1:4], iris[,5])

platform <- sessionInfo()$platform
if (substring(platform, nchar(platform)-6, nchar(platform)-1) == "64-bit") {
  test_that("OOB error of rf", {
    expect_equal(iris.rf$err.rate[500], 0.04666667, tolerance = 1e-7)
  })
}

else {
  test_that("OOB error of rf", {
    expect_equal(iris.rf$err.rate[500], , tolerance = 1e-7)
  })
}