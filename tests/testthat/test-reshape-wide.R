test_that("Reshape wide tab 1", {

  dtTab1 <- readRDS(system.file("testdata", "wide-tab01.rds", package = "orgdata"))
  dtTabSpec <- readRDS(system.file("testdata", "wide-tab01-spec.rds", package = "orgdata"))
  dtTabOut <- readRDS(system.file("testdata", "wide-tab01-out.rds", package = "orgdata"))

  expect_equal(do_reshape_wide(dtTab1, dtTabSpec), dtTabOut)

})


test_that("Reshape wide tab 2", {

  dtTab <- readRDS(system.file("testdata", "wide-tab02.rds", package = "orgdata"))
  dtTabSpec <- readRDS(system.file("testdata", "wide-tab02-spec.rds", package = "orgdata"))
  dtTabOut <- readRDS(system.file("testdata", "wide-tab02-out.rds", package = "orgdata"))

  expect_equal(do_reshape_wide(dtTab, dtTabSpec), dtTabOut)

})

test_that("Reshape wide long tab 1", {

  dtTab1 <- readRDS(system.file("testdata", "wide-long-tab1.rds", package = "orgdata"))
  dtTabSpec <- readRDS(system.file("testdata", "wide-long-tab1-spec.rds", package = "orgdata"))
  dtTabOut <- readRDS(system.file("testdata", "wide-long-tab1-out.rds", package = "orgdata"))

  expect_equal(do_reshape_long(dtTab1, dtTabSpec), dtTabOut)

})

test_that("Reshape wide long tab 2", {

  dtTab <- readRDS(system.file("testdata", "wide-long-tab2.rds", package = "orgdata"))
  dtTabSpec <- readRDS(system.file("testdata", "wide-long-tab2-spec.rds", package = "orgdata"))
  dtTabOut <- readRDS(system.file("testdata", "wide-long-tab2-out.rds", package = "orgdata"))

  expect_equal(do_reshape_long(dtTab, dtTabSpec), dtTabOut)

})

test_that("Reshape wide spec tab 1", {

  dt <- readRDS(system.file("testdata", "wide-tab01.rds", package = "orgdata"))
  dtSpec <- readRDS(system.file("testdata", "wide-file-spec.rds", package = "orgdata"))
  dtSpecOut <- readRDS(system.file("testdata", "wide-file-spec-out.rds", package = "orgdata"))

  expect_equal(get_reshape_wide_spec(dt = dt, spec = dtSpec), dtSpecOut)

})
