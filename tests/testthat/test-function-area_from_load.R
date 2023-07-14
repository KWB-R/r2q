test_that("area_from_load() works", {

  f <- r2q:::area_from_load

  expect_error(f())

  expect_identical(
    f(load_runoff = 1, Ci_storm = 1, coeff_runoff = 1, q_rain = 1, t_rain = 1),
    1000
  )
  
})
