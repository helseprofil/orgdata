
test_that("Log", {

  testthat::skip_on_covr()
  testthat::skip_on_ci()
  testthat::skip_on_cran()
  skip_if_check()
  library(data.table)
  dt <- readRDS(system.file("testdata", "wide-tab01.rds", package = "orgdata"))
  dt[, GEO := as.character(GEO)][1:2, GEO := "1010xxx"]

  notCodes <- c("100xx", "200xx")
  notCode <- "300xx"

  expect_equal(is_log_write(notCodes, "testNA1"), "`read_log(\"testNA1\")`")
  expect_equal(is_log_write(notCode, "testNA2", 123), "`read_log(\"testNA2\", koblid)`")

  output1 <- structure(list(V1 = c("100xx", "200xx")), row.names = c(NA, -2L),
                       class = c("data.table", "data.frame"))
  output2 <- structure(list(V1 = c("300xx")), row.names = c(NA, -1L),
                       class = c("data.table", "data.frame"))

  expect_equal(read_log("testNA1"), output1)
  expect_equal(read_log("testNA2", 123), output2)
})
