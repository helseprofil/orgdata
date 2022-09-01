test_that("Extra args for group", {

  ## DATA ----
  dt <- readRDS(system.file("testdata", "old-bydel.rds", package = "orgdata"))
  dout <- readRDS(system.file("testdata", "old-bydel-out.rds", package = "orgdata"))

  ## TEST -----
  expect_equal(is_delete_bydel_before_2003(dt, "DeleteOldBydel"), dout)

})

test_that("Extra age category", {

  dt <- readRDS(system.file("testdata", "AgeCatDT.rds", package = "orgdata"))
  dtout <- readRDS(system.file("testdata", "AgeCatOut.rds", package = "orgdata"))
  dtoutEven <- readRDS(system.file("testdata", "AgeCatOutEven.rds", package = "orgdata"))

  expect_error(is_input_age_class("5_"))
  expect_equal(age_category.val(dt = data.table::copy(dt), interval = 3), dtout)
  expect_equal(age_category.val(dt = data.table::copy(dt), interval = 10), dtoutEven)

})


test_that("Extra age category fix group", {

  dt <- readRDS(system.file("testdata", "AgeGrpDT.rds", package = "orgdata"))
  inx <- readRDS(system.file("testdata", "AgeGrpInterval.rds", package = "orgdata"))
  dtout <- readRDS(system.file("testdata", "AgeGrpOut.rds", package = "orgdata"))

  expect_equal(age_category.cat(dt = data.table::copy(dt), interval = inx), dtout)

})
