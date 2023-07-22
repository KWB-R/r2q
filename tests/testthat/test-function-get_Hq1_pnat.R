test_that("get_Hq1_pnat() works", {

  f <- r2q:::get_Hq1_pnat
  
  expect_identical(f(slope = 0.1, area_catch = 60), 50)
  expect_identical(f(slope = 0.1, area_catch = 3), 126)
  expect_identical(f(slope = 0.5, area_catch = 100), 100)
  expect_identical(f(slope = 0.5, area_catch = 99), 101)
  expect_identical(f(slope = 2, area_catch = 400), 150)
  expect_identical(f(slope = 2, area_catch = 300), 200)
})
