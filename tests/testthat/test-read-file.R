test_that("Read rawdata", {
  expect_error(find_data("raw-data.spss"))
  expect_error(read_file.default("raw-data.spss"))
  expect_error(read_file("raw-data.spss"))
})

test_that("Args for find_data", {

  dotcsv <- list(header = TRUE, skip = "0", nrows = "10", trimws = TRUE)
  dotxls <- list(header = TRUE, skip = "0", sheet = "Sheet1", nrows = "10")
  elm <- c("skip", "nrows")
  outws <- list(header = TRUE, skip = 0, nrows = 10, trimws = TRUE)
  outcsv <- list(header = TRUE, skip = 0, nrows = 10, strip.white = TRUE)
  outxls <- list(col_names = TRUE, skip = 0, sheet = "Sheet1", n_max = 10)

  expect_equal(is_numeric_args(dotcsv, elm), outws)
  expect_equal(is_dt_args(dotcsv), outcsv)
  expect_equal(is_xls_args(dotxls), outxls)

  ut <- list(fill=TRUE)
  expect_equal(is_args(list(fill=TRUE)), ut)
  expect_equal(is_args(fill=TRUE), ut)
})

test_that("File path", {
  fcsv <- "C:\\TEST\\file.csv"
  fcsv2 <- "C:/TEST/file.csv"

  expect_equal(is_file_path(fcsv), fcsv)
  expect_equal(is_file_path(fcsv2), fcsv2)
})
