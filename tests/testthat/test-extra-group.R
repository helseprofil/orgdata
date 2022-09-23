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

  expect_error(find_age_category(dt = cars, "bla"))
  expect_error(is_input_age_class("5_"))
  expect_equal(find_age_category.val(dt = data.table::copy(dt), interval = 3), dtout)
  expect_equal(find_age_category.val(dt = data.table::copy(dt), interval = 10), dtoutEven)

})


test_that("Extra age category fix group", {

  dt <- readRDS(system.file("testdata", "AgeGrpDT.rds", package = "orgdata"))
  inx <- readRDS(system.file("testdata", "AgeGrpInterval.rds", package = "orgdata"))
  dtout <- readRDS(system.file("testdata", "AgeGrpOut.rds", package = "orgdata"))
  extra <- "AgeCat(0,10,40,70)"

  expect_equal(is_age_category(dt = data.table::copy(dt), extra = extra), dtout)
  expect_equal(find_age_category.cat(dt = data.table::copy(dt), interval = inx), dtout)

})

test_that("Create age codebook", {

  codebook <- readRDS(system.file("testdata", "AgeCodebook.rds", package = "orgdata"))

  expect_equal(is_age_codebook(sample(1:33, 20, replace = TRUE), c(1, 5, 10, 15, 20, Inf)), codebook)
})

test_that("Create age category mix group", {

  input <- "AgeCat(0,5, [10], 55, 60)"
  dtout <- readRDS(system.file("testdata", "MixOut.rds", package = "orgdata"))

  expect_equal(is_input_age_class(input), dtout)
})
