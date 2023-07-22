test_that("plot_hazards() works", {

  f <- r2q::plot_hazards

  expect_error(f())

  hazards = list(1)
  
  f(hazards)
})
