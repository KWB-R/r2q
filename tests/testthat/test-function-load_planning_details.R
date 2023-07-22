test_that("load_planning_details() works", {

  f <- r2q::load_planning_details

  expect_error(f())

  expect_error(
    f(data.dir = tempdir(), filename = "no-such-file"),
    "File does not exist"
  )

  file <- tempfile("test-", fileext = ".xlsx")
  
  write_xls <- function(data) {
    writexl::write_xlsx(list(a = data), file)
  }
  
  data <- data.frame(
    A = c(NA, "f_id", 1:3), 
    B = c(NA, "area_m2", 2:4)
  )
  
  write_xls(data)
  
  f(data.dir = dirname(file), filename = basename(file))

  data <- rbind(data, data.frame(A = 4, B = NA))

  write_xls(data)  
  
  expect_warning(
    f(data.dir = dirname(file), filename = basename(file))
  )
  
})
