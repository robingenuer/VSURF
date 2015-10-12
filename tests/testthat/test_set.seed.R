context("randomness in runif and randomForest")

platform <- sessionInfo()$platform
is.64 <- function(platform) {
  if (substring(platform, nchar(platform) - 6, nchar(platform) - 1) == "64-bit") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

set.seed(2219)
data(iris)
iris.rf <- randomForest(iris[,1:4], iris[,5])

test_that("OOB error of rf", {
  expect_equal(iris.rf$err.rate[500],
               ifelse(is.64(platform), 0.04666667, 0.04), tolerance = 1e-7)
})