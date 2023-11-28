library(testthat)


source("~/SimpleLinearRegression101/R/LoadData.R")

test_that("Appropriate Errors while Loading Data", {
  expect_error(Load(abcsde))
  expect_error(Load(abcde.tva))
})



