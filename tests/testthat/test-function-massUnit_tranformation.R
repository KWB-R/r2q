test_that("massUnit_tranformation() works", {

  f <- r2q::massUnit_tranformation

  expect_error(f())

  expect_identical(f("ng", change = 0L), "ng")
  expect_identical(f("ng", change = 1L), "ug")
  expect_identical(f("ng", change = 2L), "mg")
})
