test_that("get_x() works", {

  f <- r2q::get_x
  
  expect_error(f())

  expect_identical(f(1, NULL), 0.1)
  expect_identical(f(100, NULL), 0.1)
  
  expect_identical(f(1, 1), 0)
  expect_identical(f(0, 1), Inf)
})
