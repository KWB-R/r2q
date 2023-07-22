test_that("merge_by_pollutant() works", {

  f <- r2q::merge_by_pollutant
  
  expect_error(f())

  df1 <- data.frame(substance = "a", unit = "b", x = 1)
  df1a <- data.frame(Substance = "a", Unit = "b", x = 1)
  
  df2 <- data.frame(substance = "a", unit = "b", y = 2)
  df3 <- data.frame(substance = "a", unit = "b", x = 1, y = 2)
  
  expect_identical(f(df1, df2), df3)
  expect_identical(f(df1a, df2), df3)
})

