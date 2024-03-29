test_that("Post recode raw", {

  dt <- readRDS(system.file("testdata", "post-recode-dt.rds", package = "orgdata"))
  spec <- structure(list(FILGRUPPE = "TEST01", KOL = "INNVKAT", TYPE = "PS",
                         FRA = "raw(AAR == 2014 & LANDBAK %chin% c(\"0\", \"20\"))",
                         TIL = ".."), row.names = 1L, class = "data.frame")

  input <- "raw(AAR == 2014 & LANDBAK %chin% c(\"0\", \"20\"))" #balanced parenthesis
  input2 <- "raw(AAR == 2014 & LANDBAK %chin% c(\"0\", \"20\")" #unbalanced parenthesis
  input3 <- "raw(AAR == 2014 & LANDBAK %chin% c(\"0\", \"20\")))" #unbalanced parenthesis on rhs
  recodeCol <- "INNVKAT"
  toVAL <- ".."
  dtOut <- readRDS(system.file("testdata", "post-recode-out-raw.rds", package = "orgdata"))

  expect_equal(is_recode_post(dt = data.table::copy(dt), spec = spec, input = input, recodeCol = recodeCol, toVAL = toVAL), dtOut)
  expect_equal(is_recode_post(dt = data.table::copy(dt), spec = spec, input = input2, recodeCol = recodeCol, toVAL = toVAL), dtOut)
  expect_error(is_recode_post(dt = data.table::copy(dt), spec = spec, input = input3, recodeCol = recodeCol, toVAL = toVAL))
})

test_that("Delete post row", {

  dt <- readRDS(system.file("testdata", "post-recode-dt.rds", package = "orgdata"))
  spec <- structure(list(FILGRUPPE = "TEST01", KOL = "INNVKAT", TYPE = "PS",
                         FRA = "AAR = 2014 & LANDBAK = 20",
                         TIL = "-"), row.names = 1L, class = "data.frame")

  input <- "raw(AAR == 2014 & LANDBAK == 20)" #balanced parenthesis
  recodeCol <- "INNVKAT"
  toVal = "-"
  dtDelOut <- readRDS(system.file("testdata", "post-delete-out-raw.rds", package = "orgdata"))

  expect_equal(is_post_delete_row(data.table::copy(dt), input = input, toVAL = toVal, typ = "raw"), dtDelOut)
  expect_equal(is_post_delete_row(data.table::copy(dt), spec = spec, toVAL = toVal, typ = "exp"), dtDelOut)

})


test_that("Post recode exp", {

  dt <- readRDS(system.file("testdata", "post-recode-dt.rds", package = "orgdata"))
  spec <- structure(list(FILGRUPPE = "TEST01", KOL = "INNVKAT", TYPE = "PS",
                         FRA = "GEO = 0 & AAR=2014 & LANDBAK = 20", TIL = ".."),
                    row.names = 2L, class = "data.frame")
  input <- "GEO = 0 & AAR=2014 & LANDBAK = 20"
  recodeCol <- "INNVKAT"
  toVAL <- ".."
  dtOut <- readRDS(system.file("testdata", "post-recode-out-exx.rds", package = "orgdata"))


  ## TEST

  expect_equal(is_recode_post(dt = dt, spec = spec, input = input, recodeCol = recodeCol, toVAL = toVAL), dtOut)
})


test_that("Post numeric is string", {

  specStr <- "GEO = 0 & AAR=2014 & LANDBAK = 20"
  out <- "Post recode codebook `FRA` is not numeric for AAR"

  expect_error(is_post_exp(specStr))
})
