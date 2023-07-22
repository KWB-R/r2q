test_that("run_scenario() works", {

  f <- r2q::run_scenario

  expect_error(f())

  file <- tempfile("test-", fileext = ".xlsx")

  status_quo_list <- list(
    input_path = dirname(file),
    filename = basename(file),
    rain = 1:2, 
    siteData = list(
      rain_year = list(
        Value = 1
      )
    ) 
  )
  
  data <- data.frame(
    A = c(NA, "f_id", 1:3), 
    B = c(NA, "area_m2", 2:4),
    C = c(NA, "suspended_solids", 3:5),
    D = c(NA, "fD", 4:6)
  )

  data <- data.frame(
    f_id = c(NA, NA, 1:3), 
    area_m2 = c(NA, NA, 2:4),
    suspended_solids = c(NA, NA, 3:5),
    fD = c(NA, NA, 4:6)
  )
  
  openxlsx::write.xlsx(list(a = data), file, )

  f(status_quo_list, scenario_name = "a")
})
