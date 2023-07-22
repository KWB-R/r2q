test_that("get_KOSTRA() works", {

  f <- r2q:::get_KOSTRA
  
  expect_error(f())

  f(
    coord_vector = c(3813634.44, 2753912.5),
    duration_string = "10"
  )
})
