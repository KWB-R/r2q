test_that("substr_reverse() works", {

  f <- r2q::substr_reverse

  expect_error(f())

  expect_identical(
    f("abcd", rev_start = 2, rev_stop = 3, keep = TRUE), 
    "bc"
  )
  
  expect_identical(
    f("abcd", rev_start = 2, rev_stop = 3, keep = FALSE), 
    "a"
  )
})
