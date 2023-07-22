test_that("maxArea_event() works", {

  f <- r2q::maxArea_event

  expect_error(f())

  result <- f(
    Q_river = 1, 
    Ci_river = 1, 
    Ci_threshold = 1, 
    Ci_storm = 1, 
    coeff_runoff = 1, 
    q_rain = 1, 
    t_rain = 1, 
    river_length = 1, 
    river_cross_section = 1,
    catchment_area = 100
  )

  expect_type(result, "double")
  expect_length(result, 1L)
  
  expect_error(maxArea_event(
    Q_river = 1, 
    Ci_river = 1, 
    Ci_threshold = "acute", 
    Ci_storm = 1, 
    coeff_runoff = 1,
    q_rain = 1, 
    t_rain = 1, 
    river_length = 100,
    river_cross_section = 100
  ))
  
})
