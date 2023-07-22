#library(testthat)
test_that("maxArea_year() works", {

  f <- r2q::maxArea_year

  expect_error(f())

  expect_identical(100, f(
    load_max = 1,
    Ci_threshold = 1, 
    Ci_storm = 1, 
    coeff_runoff = 1, 
    Q_rain = 1
  ))
})
