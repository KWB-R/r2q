test_that("sub_OgRe_to_name() works", {

  f <- r2q::sub_OgRe_to_name

  expect_error(f())

  expect_message(f(c_table = data.frame(substance = "x")))

  c_table <- data.frame(substance = "Anthracen")
  
  result <- f(c_table)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 1L)
  
  result <- f(c_table, all_substances = FALSE)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) == 1L)
})
