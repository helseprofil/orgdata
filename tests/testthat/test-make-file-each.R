test_that("Make file each long", {

  dt <- readRDS(system.file("testdata", "wide-long-col.rds", package = "orgdata"))
  dtSpec <- readRDS(system.file("testdata", "wide-long-col-spec.rds", package = "orgdata"))
  dtWideSpec <- readRDS(system.file("testdata", "wide-long-col-widespec.rds", package = "orgdata"))
  dtout <- readRDS(system.file("testdata", "wide-long-col-out.rds", package = "orgdata"))

  expect_equal(is_long_col(dt, dtSpec, dtWideSpec), dtout)


})
