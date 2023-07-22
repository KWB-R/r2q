test_that("run_status_quo() works", {

  f <- r2q::run_status_quo

  expect_error(f())

  expect_error(
    f(
      path = system.file("extdata/Example", package = "r2q"),
      filename = "Herne_Baukau.xlsx", 
      c_type = "a"
    )
  )
})
