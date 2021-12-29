test_that("Make file simple", {

  testthat::skip_on_covr()
  testthat::skip_on_ci()
  testthat::skip_on_cran()
  simpleDT <- readRDS(system.file("testdata_dev", "make-file-dt.rds", package = "orgdata"))
  leseOUT <- readRDS(system.file("testdata_dev", "make-file2-dt.rds", package = "orgdata"))
  debug_opt("nrow", 500)
  expect_equal(make_file("ENPERSON", 272), simpleDT)
  expect_equal(make_file("LESEFERD", 144), leseOUT)
  reset_opt()



})
