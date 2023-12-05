library(testthat)


x <- rnorm(100)
y <- 2*x + 5

table <- cbind(x,y)

testthat::test_that("Appropriate Errors while Loading Data", {
  expect_error(Load(abcsde))
  expect_error(Load(abcde.tva))
})


testthat::test_that("Appropriate data type output from Simple Linear", {
  expect_s3_class(SimpleLinear(x,y), "summary.lm")
})
