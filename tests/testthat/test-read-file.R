test_that("Read rawdata", {
  expect_error(find_data("raw-data.spss"))
  expect_error(read_file.default("raw-data.spss"))
  expect_error(read_file("raw-data.spss"))
})

test_that("Args for find_data", {

  dotcsv <- list(header = TRUE, skip = "0", sheet = "Sheet1", nrows = "10")
  dotxls <- list(header = TRUE, skip = "0", sheet = "Sheet1", n_max = "10")
  elm <- c("skip", "nrows")
  outcsv <- list(header = TRUE, skip = 0, sheet = "Sheet1", nrows = 10)
  outxls <- list(header = TRUE, skip = 0, sheet = "Sheet1", n_max = 10)

  expect_equal(is_convert_var(dotcsv, elm), outcsv)
  expect_equal(is_dt_var(dotcsv), outcsv)
  expect_equal(is_xls_var(dotxls), outxls)

})
