
test_that("Select only valid files", {

  # Data ---------------------------------------------------
  specDT <- structure(list(KOBLID = c(18L, 14L), FILID = c(13L, 5L), MANHEADER = c(
    NA,
    "5=UTDANN"
  ), IBRUKTIL = structure(c(2932532, 10957), class = "Date")), class = c(
    "data.table",
    "data.frame"
  ), row.names = c(NA, -2L))


  brukDT <- structure(list(
    KOBLID = 18L, FILID = 13L, MANHEADER = NA_character_,
    IBRUKTIL = structure(2932532, class = "Date")
  ), class = c(
    "data.table",
    "data.frame"
  ), row.names = c(NA, -1L))

  oldDT <- structure(list(
    KOBLID = 14L, FILID = 5L, MANHEADER = "5=UTDANN",
    IBRUKTIL = structure(10957, class = "Date")
  ), class = c(
    "data.table",
    "data.frame"
  ), row.names = c(NA, -1L))

  expect_equal(is_org_files(spec = specDT, id = NULL), data.table::setDF(brukDT))
  expect_equal(is_org_files(spec = specDT, 18), data.table::setDF(brukDT))
  expect_error(is_org_files(spec = specDT, 14), "No valid file found!")
})


test_that("Columns and class data", {
  ## Data ----------------

  dataRaw <- structure(list(AAR = c(2019L, 2019L, 2019L),
                            GEO = c(3013421L, 50012111L, 50012219L),
                            KJONN = c("2", "2", "2"), ALDER = c(89L, 93L, 94L),
                            UTDANN = c("2", "1", "1"), LANDSSB = c("0", "0", "0"),
                            SIVILSTAND = c(3L, 3L, 3L), VAL1 = c(4L, 4L, 4L),
                            LANDB = c(9L, 9L, 9L), LANDF = c(1L, 1L, 1L)),
                    row.names = c(NA, -3L), class = c("data.table", "data.frame"))

  dataOut <- structure(list(AAR = c(2019L, 2019L, 2019L),
                            GEO = c(3013421L, 50012111L, 50012219L),
                            KJONN = c(2L, 2L, 2L), ALDER = c(89L, 93L, 94L),
                            UTDANN = c(2L, 1L, 1L), LANDSSB = c("0", "0", "0"),
                            SIVILSTAND = c(3L, 3L, 3L), VAL1 = c(4L, 4L, 4L),
                            LANDB = c(9L, 9L, 9L), LANDF = c(1L, 1L, 1L)),
                       row.names = c(NA, -3L), class = c("data.table", "data.frame"))

  fgspec <- structure(list(ID = 10L, FILGRUPPE = "Dode", AGGREGERE = "F,K",
                           KOLNAVN = "sivil = SIVILSTAND", ADDVAL = NA_character_,
                           NAVAL = NA_character_, SPLITTFRA = "LANDSSB", SPLITTTIL = "LANDB, LANDF"),
                      class = "data.frame", row.names = c(NA, -1L))

  cols <- c("GEO", "AAR", "KJONN", "ALDER", "UTDANN", "LANDSSB", "TAB1",
            "TAB2", "TAB3", "VAL1", "VAL2", "VAL3", "SIVILSTAND", "LANDB",
            "LANDF")

  fgspecMis <- structure(list(ID = 10L, FILGRUPPE = "Dode", AGGREGERE = "F,K",
                              KOLNAVN = NA_character_, ADDVAL = NA_character_,
                              NAVAL = NA_character_, SPLITTFRA = NA_character_, SPLITTTIL = NA_character_),
                         class = "data.frame", row.names = c(NA,-1L))
  colmis <- c("GEO", "AAR", "KJONN", "ALDER", "UTDANN", "LANDSSB", "TAB1",
              "TAB2", "TAB3", "VAL1", "VAL2", "VAL3")

  ## TEST -------------------
  expect_equal(is_col_int(dataRaw), dataOut)
  expect_equal(is_data_cols(fgspec = fgspec), cols)
  expect_equal(is_data_cols(fgspec = fgspecMis), colmis)
})


test_that("Geo level", {
  expect_equal(is_geo_level(2), "f")
  expect_equal(is_geo_level(12345678), "g")
  expect_equal(is_geo_level(1234567), "g")
  expect_equal(is_geo_level(1234), "k")
  expect_equal(is_geo_level(123), "k")
})
