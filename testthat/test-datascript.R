library(testthat)
context("checks that datascript is working")

test_that("printed value is correct", {
  
  expect_output(str(source("./datascript.R")), 
                   "Updating feeder data")

})
