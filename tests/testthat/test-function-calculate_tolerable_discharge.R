test_that("calculate_tolerable_discharge() works", {

  f <- r2q::calculate_tolerable_discharge
  
  check <- function(result) {
    expect_s3_class(result, "data.frame")
    expect_identical(nrow(result), 1L)
  }
  
  expect_output(result <- f(verbose = TRUE))
  check(result)

  expect_silent(result <- f(verbose = FALSE))
  check(result)
})
