# test_that("Make file simple", {
# 
#   testthat::skip_on_covr()
#   testthat::skip_on_ci()
#   testthat::skip_on_cran()
#   skip_if_check()
#   ## simpleDT <- readRDS(system.file("testdata_dev", "make-file-dt.rds", package = "orgdata"))
#   ## leseOUT <- readRDS(system.file("testdata_dev", "make-file2-dt.rds", package = "orgdata"))
#   ## debug_opt("nrow", 500)
#   ## expect_equal(make_file("ENPERSON", 272, year = 2021), simpleDT)
#   ## expect_equal(make_file("LESEFERD", 144, year = 2021), leseOUT)
#   reset_opt()
# })

test_that("Select files", {

  sel <- readRDS(file = system.file("testdata", "spec-file.rds", package = "orgdata"))
  out <- readRDS(file = system.file("testdata", "spec-file-output.rds", package = "orgdata"))
  last <- readRDS(file = system.file("testdata", "last-file-output.rds", package = "orgdata"))

  expect_equal(is_select_file(spec = sel, select = 30:34), out)
  expect_equal(is_select_file(spec = sel, select = "last"), last)
  expect_equal(is_select_file(spec = sel[30:34,], select = NULL, rowfile = 5), out)
})
