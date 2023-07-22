test_that("Input_event() works", {

  f <- r2q::Input_event

  expect_error(f())

  expect_identical(
    f(
      area_runoff = 0,
      Ci_storm = 0, 
      coeff_runoff = 0, 
      q_rain = 0, 
      t_rain = 0
    ), 
    0
  )
  
  expect_identical(
    f(
      area_runoff = 1000,
      Ci_storm = 1, 
      coeff_runoff = 1, 
      q_rain = 1, 
      t_rain = 1
    ), 
    1
  )
  
})
