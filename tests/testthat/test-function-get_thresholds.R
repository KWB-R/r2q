#library(testthat)
test_that("get_thresholds() works", {

  f <- r2q::get_thresholds
  
  result <- f()

  expect_s3_class(result, "data.frame")
  
  expect_identical(names(result), c(
    "s_id", 
    "substance", 
    "Unit", 
    "threshold", 
    "threshold_type", 
    "Source"
  ))
  
  expect_error(f(SUW_type = "other"))
})
