test_that("read_csv_utf8() works", {

  f <- r2q:::read_csv_utf8

  expect_error(f())

  file <- system.file("extdata/IDs/substance_id.csv", package = "r2q")
  
  result <- f(file)
  
  expect_s3_class(result, "data.frame")
  
  expect_identical(names(result), c(
    "s_id",
    "substance",
    "name_OgRe",
    "name_de",
    "name_en",
    "group_de",
    "group_en"
  ))
})
