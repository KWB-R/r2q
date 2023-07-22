test_that("plot_connectable_urban_area() works", {

  f <- r2q::plot_connectable_urban_area

  expect_error(f())

  r2q_substance <- list(
    general = list(
      area_pot_rel = 1
    )
  )

  r2q_hydrology <- list(
    discharge_parameters = list(
      Value = 1:6
    )
  ) 
  
  expect_error(f(
    r2q_substance = r2q_substance, 
    site_data = list(),
    r2q_hydrology = r2q_hydrology
  ))
  
  expect_error(f(
    r2q_substance = r2q_substance, 
    site_data = list(),
    r2q_hydrology = r2q_hydrology,
    x_type = "ha"
  ))
  
})
