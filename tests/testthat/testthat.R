# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(SimpleLinearRegression101)

test_check("SimpleLinearRegression101")

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

