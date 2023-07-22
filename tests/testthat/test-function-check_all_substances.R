test_that("check_all_substances() works", {

  f <- r2q:::check_all_substances

  expect_error(f())
  expect_type(f(c_table = data.frame(), c_type = "average"), "list")
  expect_type(f(c_table = data.frame(), c_type = "worstcase"), "list")
})
