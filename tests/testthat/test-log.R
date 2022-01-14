
test_that("Log", {

  testthat::skip_on_covr()
  testthat::skip_on_ci()
  testthat::skip_on_cran()
  skip_if_check()
  library(data.table)
  dt <- readRDS(system.file("testdata", "wide-tab01.rds", package = "orgdata"))
  dt[, GEO := as.character(GEO)][1:2, GEO := "1010xxx"]

  notCodes <- c("100xx", "200xx")
  expect_equal(is_log_write(notCodes, "testNA1"), "`read_log(\"testNA1\")`")
  expect_equal(is_log_write(notCodes, "testNA2", 123), "`read_log(\"testNA2\", koblid)`")

  output <- structure(list(`100xx` = "200xx"), row.names = c(NA, -1L),
                      class = c("data.table", "data.frame"))

  expect_equal(read_log("testNA1"), output)
  expect_equal(read_log("testNA2"), output)
})
