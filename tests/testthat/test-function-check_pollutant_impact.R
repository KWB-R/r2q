test_that("check_pollutant_impact() works", {

  f <- r2q::check_pollutant_impact

  expect_error(f())

  expect_identical(-Inf, f(Ci_river = 1, Ci_threshold = 0, Ci_storm = 1))
  expect_identical(Inf, f(Ci_river = 1, Ci_threshold = 1, Ci_storm = 1))
  expect_true(f(Ci_river = 1, Ci_threshold = 1, Ci_storm = 2))
})
