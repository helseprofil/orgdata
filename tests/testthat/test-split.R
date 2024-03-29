test_that("Get split input", {
  fsplit <- list(from = "LANDSSB", to = c("landb", "landf"))
  data.table::setDT(DT01)

  dataInn <- data.table::copy(dataSplit)
  outSplit <- structure(list(GEO = c(3010102L, 3010102L, 3010102L, 3010102L, 3010102L),
                             KJONN = c(1L, 1L, 1L, 1L, 1L),
                             ALDER = c(0L, 1L, 4L, 6L, 18L), LANDSSB = c("1C", "0", "1C", "3B", "0"),
                             VAL1 = c(1L, 1L, 1L, 1L, 1L), LANDB = c("1", "0", "1", "3", "0"),
                             INNVKAT = c("C", "0", "C", "B", "0")),
                        row.names = c(NA, -5L), class = c("data.table"))
  data.table::setDT(outSplit)

  expect_equal(get_split(spec = befolk), fsplit)
  expect_error(get_split())

  expect_equal(do_split(dataInn, spt), outSplit)
  expect_error(do_split(DT01, data.table::setDF(fsplit)))
})

test_that("Duplicate split input", {

  ## Data --------------
  dSpt <- data.table::copy(dataSplit)
  out <- structure(list(GEO = c(3010102L, 3010102L, 3010102L, 3010102L, 3010102L),
                        KJONN = c(1L, 1L, 1L, 1L, 1L),
                        ALDER = c(0L, 1L, 4L, 6L, 18L),
                        LANDSSB = c("1C", "0", "1C", "3B", "0"),
                        VAL1 = c(1L, 1L, 1L, 1L, 1L),
                        LANDSSB_split = c("1C", "00", "1C", "3B", "00")),
                   row.names = c(NA, -5L), class = c("data.frame"))
  data.table::setDT(out)

  erdata <- data.table::copy(dataSplit)
  erdata[LANDSSB == "1C", LANDSSB := "1CC"]

  ## TEST -------------------
  expect_equal(is_split_check(dt = dSpt, split = spt), out)
  expect_error(is_split_check(dt = erdata, split = spt))
})

test_that("Split common variable", {

  # Data ---------
  dts <- readRDS(system.file("testdata", "splitdt_err.rds", package = "orgdata"))
  code_err <- readRDS(system.file("testdata", "splitcode_err.rds", package = "orgdata"))
  dtout <- readRDS(system.file("testdata", "dtout.rds", package = "orgdata"))
  dtCom <- readRDS(system.file("testdata", "dtout_comm.rds", package = "orgdata"))

  expect_equal(is_delete_lesid(dt = copy(dts), code_err, "v1"), dtout)
  expect_equal(is_delete_common(dt = copy(dts), code_err, "LES_TEST"), dtCom)
})
