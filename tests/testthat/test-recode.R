test_that("Recode variables", {

  ## recode data and spec
  dtCB <- structure(list(AAR = c(2019L, 2019L, 2019L, 2019L),
                         LANDSSB = c("1B", "3C", "3B", "0"),
                         LANDB = c("1", "3", "3", "1"),
                         LANDF = c("B", "C", "B", NA_character_ )), row.names = c(NA, -4L),
                    class = c("data.table", "data.frame"))

  specCB <- structure(list(FILGRUPPE = c("ALLE", "test002", "ALLE", "test002", "test002", NA),
                           LESID = c(NA, NA, NA, "ver02", NA, "ver03"), KOL = c("LANDF", "LANDF", "LANDF", "LANDF", "LANDF", "LANDF"),
                           TYPE = c(1L, 1L, 1L, 1L, 1L, 1L), FRA = c("B", "C", "<NA>", "B", "B", "B"),
                           TIL = c("2", "3", "1", "8", "9", "9")), class = c("data.table", "data.frame"),
                      row.names = c(NA, -5L))

  ## output lesid
  outLesid <- structure(list(AAR = c(2019L, 2019L, 2019L, 2019L),
                             LANDSSB = c("1B", "3C", "3B", "0"),
                             LANDB = c("1", "3", "3", "1"),
                             LANDF = c("8", "C", "8", NA_character_)),
                        row.names = c(NA, -4L),
                        class = c("data.table","data.frame"))

  ## output recode_common
  outCommon <- structure(list(AAR = c(2019L, 2019L, 2019L, 2019L),
                              LANDSSB = c("1B", "3C", "3B", "0"),
                              LANDB = c("1", "3", "3", "1"),
                              LANDF = c("9", "3", "9", NA_character_)),
                         row.names = c(NA, -4L),
                         class = c("data.table","data.frame"))

  ## ouput recode_all
  outAll <- structure(list(AAR = c(2019L, 2019L, 2019L, 2019L),
                           LANDSSB = c("1B", "3C", "3B", "0"),
                              LANDB = c("1", "3", "3", "1"),
                           LANDF = c("2", "C", "2", "1")),
                      row.names = c(NA, -4L),
                         class = c("data.table","data.frame"))
  ## is_NA
  dtt <- structure(list(VAL = c(1L, 1L), LANDB = c("1", "0"), LANDF = c("B", NA)),
                   row.names = c(NA, -2L), class = c("data.table", "data.frame"))

  dtout <- structure(list(VAL = c(1L, 1L), LANDB = c("1", "0"), LANDF = c("B", "<NA>")),
                     row.names = c(NA, -2L), class = c("data.table", "data.frame"))

  code <- structure(list(KOL = c("LANDF", "LANDF", "LANDF"),
                         FRA = c("B", "C", "<NA>"),
                         TIL = c("2", "3", "1")),
                    row.names = c(NA, -3L), class = c("data.table", "data.frame"))

  expect_equal(is_recode_lesid(dt = data.table::copy(dtCB), code = specCB, lesid = "ver02"), outLesid)
  expect_equal(is_recode_common(dt = data.table::copy(dtCB), code = specCB, group = "test002"), outCommon)
  expect_equal(is_recode_all(dt = data.table::copy(dtCB), code = specCB), outAll)
  expect_error(is_codebook(cb = specCB))
  expect_equal(is_NA(dt = dtt, code = code, col = "LANDF"), dtout)
})

test_that("Recode duplicate", {

  dt <- readRDS(system.file("testdata", "RecodeDupDT.rds", package = "orgdata" ))
  code <- readRDS(system.file("testdata", "RecodeDupCode.rds", package = "orgdata" ))

  expect_error(is_recode_lesid(dt, code, "vers2"))
})
