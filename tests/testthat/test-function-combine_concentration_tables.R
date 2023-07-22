test_that("combine_concentration_tables() works", {

  f <- r2q::combine_concentration_tables
  
  expect_error(f())

  threshold_table <- data.frame(
    substance = "a",
    Unit = "b",
    threshold = 1,
    threshold_type = "c"
  )
  
  storm_table <- data.frame(
    substance = "a",
    unit = "b",
    x = 1
  )
  
  background_table <- data.frame(
    substance = "a",
    unit = "b",
    y = 2
  )
  
  result <- f(
    threshold_table = threshold_table,
    storm_table = storm_table, 
    background_table = background_table
  )
  
  expect_identical(result, data.frame(
    substance = "a",
    unit = "b",
    threshold = 1,
    threshold_type = "c",
    c_river = 2,
    x = 1
  ))
})
