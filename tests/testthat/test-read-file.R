test_that("Read rawdata", {

  dtOut <- readRDS(system.file("testdata", "testfile_read.rds", package = "orgdata"))
  csvOut <- readRDS(system.file("testdata", "csvOut.rds", package = "orgdata"))
  xlOut <- readRDS(system.file("testdata", "xlOut.rds", package = "orgdata"))

  expect_equal(read_file(file.path(system.file(package = "orgdata"), "testdata/testfile.xlsx"), skip = 1, range = "A2:B6"), dtOut)
  expect_equal(find_data.csv(system.file("testdata", "cars.csv", package = "orgdata"), nrows = 10, header = FALSE, skip = 1), csvOut)
  expect_equal(find_data.xlsx(system.file("testdata", "testfile.xlsx", package = "orgdata"), col_names=FALSE, skip=2, n_max=3), xlOut)
  expect_error(find_data("raw-data.spss"))
  expect_error(read_file.default("raw-data.spss"))
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

  expect_equal(is_read_path(fcsv), fcsv)
  expect_equal(is_read_path(fcsv2), fcsv2)
})


test_that("Dots args", {

  Dots <- list(trimws = TRUE, nrows = 50)
  csvOut <- list(strip.white = TRUE, nrows = 50)
  xlsOut <- list(trim_ws = TRUE, n_max = 50)

  expect_equal(is_csv_dots(Dots), csvOut)
  expect_equal(is_xls_dots(Dots), xlsOut)
  })

