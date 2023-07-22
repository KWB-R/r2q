test_that("immission_assessment() works", {

  f <- r2q::immission_assessment

  expect_error(f())

  site_data <- list(
    area_urban_connected = list(Value = 1),
    area_urban_connectable = list(Value = 1),
    area_plan = list(Value = 1),
    area_urban = list(Value = 1),
    f_D_is = list(Value = 1),
    f_D_pot = list(Value = 1),
    Q_event = list(Value = 1),
    river_length = list(Value = 100),
    cross_section_event = list(Value = 100)
  )
  
  c_table <- data.frame(
    substance = c("x", "y"),
    threshold_type = c("acute", "annual"),
    threshold = c(0.1, 0.2),
    unit = c("ng", "mg"),
    pot_med = c(1, 2),
    is_med = c(1, 2),
    is_q95 = c(3, 4),
    c_river = c(1, 2)
  )
  
  hazard_list <- list(
    x = c(pot_med = 1, pot_q95 = 2),
    y = c(pot_med = 3, pot_q95 = 4)
  )
  
  q_rain <- 1
  t_rain <- 1
  
  result_1 <- f(
    site_data = site_data, 
    c_table = c_table, 
    q_rain = 1, 
    t_rain = 1, 
    substance = "x", 
    hazard_list = hazard_list, 
    c_type = "average"
  )

  result_2 <- f(
    site_data = site_data, 
    c_table = c_table, 
    q_rain = 1, 
    t_rain = 1, 
    substance = "y", 
    hazard_list = hazard_list, 
    c_type = "average"
  )

  expect_type(result_1, "list")  
  expect_type(result_2, "list")  
  
})
