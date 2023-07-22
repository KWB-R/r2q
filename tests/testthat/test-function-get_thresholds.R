#library(testthat)
test_that("get_thresholds() works", {

  result  <- r2q:::get_thresholds()

  expect_s3_class(result, "data.frame")
  
  expect_identical(names(result), c(
    "s_id", 
    "substance", 
    "Unit", 
    "threshold", 
    "threshold_type", 
    "Source"
  ))
})
