test_that("sub_id_to_name() works", {

  f <- r2q::sub_id_to_name

  expect_error(f())

  c_table <- data.frame(s_id = 1)
  
  result <- f(c_table)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 1L)
  
  result <- f(c_table, all_substances = FALSE)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) == 1L)
})
