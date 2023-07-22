test_that("maxInput_year() works", {

  f <- r2q::maxInput_year

  expect_error(f())

  expect_identical(94608, f(
    Q_river = 1, 
    Ci_river = 1, 
    Ci_storm = 3, 
    Ci_threshold = 2
  ))
})
