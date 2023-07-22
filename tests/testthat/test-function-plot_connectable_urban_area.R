test_that("plot_connectable_urban_area() works", {

  f <- r2q::plot_connectable_urban_area

  expect_error(f())

  r2q_substance <- list(
    general = list(
      area_pot_rel = 1,
      areaUrban_pot_ha = 10
    )
  )

  r2q_hydrology <- list(
    discharge_parameters = list(
      Value = 1:6
    )
  ) 

  site_data <- list(
    area_plan = list(Value = 1),
    area_urban_connectable = list(Value = 1)
  )
  
  f(
    r2q_substance = r2q_substance, 
    site_data = site_data,
    r2q_hydrology = r2q_hydrology
  )
  
  f(
    r2q_substance = r2q_substance, 
    site_data = site_data,
    r2q_hydrology = r2q_hydrology,
    x_type = "ha"
  )
  
})
