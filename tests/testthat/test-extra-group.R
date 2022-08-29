test_that("Extra args for group", {

  ## DATA ----
  dt <- readRDS(system.file("testdata", "old-bydel.rds", package = "orgdata"))
  dout <- readRDS(system.file("testdata", "old-bydel-out.rds", package = "orgdata"))

  ## TEST -----
  expect_equal(is_delete_bydel_before_2003(dt, "DeleteOldBydel"), dout)

})

test_that("Extra age category", {

  dt <- readRDS(system.file("testdata", "AgeCatDT.rds", package = "orgdata"))
  inx <- readRDS(system.file("testdata", "AgeCatInterval.rds", package = "orgdata"))
  dtout <- readRDS(system.file("testdata", "AgeCatOut.rds", package = "orgdata"))

  expect_equal(age_category.val(dt, interval = inx), dtout)
})
