test_that("Extra args for group", {

  dt <- readRDS(system.file("testdata", "old-bydel.rds", package = "orgdata"))
  dout <- readRDS(system.file("testdata", "old-bydel-out.rds", package = "orgdata"))

  expect_equal(is_delete_bydel_before_2003(dt, "DeleteOldBydel"), dout)

})
