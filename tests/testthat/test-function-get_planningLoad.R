test_that("get_planningLoad() works", {

  f <- r2q::get_planningLoad

  expect_error(f())

  thresholdTable_1 <- data.frame(
    substance = "AFS_63",
    threshold_type = "acute"
  ) 

  thresholdTable_2 <- data.frame(
    substance = "AFS_63",
    threshold_type = "annual"
  ) 
  
  planning_data <- data.frame(
    suspended_solids = 1,
    f_id = 1L,
    fD = 2,
    area_m2 = 100
  )
  
  function_c_table <- data.frame(
    s_id = 1L,
    X1 = 1
  )
  
  expect_error(
    f(
      planning_data = NULL,
      sID = 1L,
      fID = 1L,
      q_rain = 1,
      t_rain = 1,
      y_rain = 1,
      thresholdTable = thresholdTable_1,
      function_c_table = function_c_table
    ), 
    regexp = "is not defined"
  )
  
  f(
    planning_data = planning_data,
    sID = 1L,
    fID = 1L,
    q_rain = 1,
    t_rain = 1,
    y_rain = 1,
    thresholdTable = thresholdTable_1,
    function_c_table = function_c_table
  )

  f(
    planning_data = planning_data,
    sID = 1L,
    fID = 1L,
    q_rain = 1,
    t_rain = 1,
    y_rain = 1,
    thresholdTable = thresholdTable_2,
    function_c_table = function_c_table
  )
  
})
