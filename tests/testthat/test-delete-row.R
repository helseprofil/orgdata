test_that("Delete rows", {

  ## DATA ------------------
  data <- structure(list(AAR = c("2006_2012", "2006_2012", "2006_2012",
                                 "2006_2012", "2006_2012", "2006_2012"),
                         GEO = c(0L, 15L, 15L, 0L, 0L, 0L), KJONN = c(0L, 0L, 0L, 0L, 0L, 0L),
                         UTDANN = c(0L, 0L, 0L, 0L, 0L, 0L),
                         LANDSSB = c("1B", "1C", "2B", "2C", "Tot", "Tot"),
                         VAL1 = c(699L, 187L, 271L, 113L, 674L, 841L)),
                    row.names = c(NA, -6L),
                    class = c("data.table", "data.frame"))


  specRow <- structure(list(FILGRUPPE = c("VGS", "VGS"),
                            LESID = c("vers1", "vers1"), KOL = c("GEO", "LANDSSB"),
                            TYPE = c(1L, 1L), FRA = c("0", "Tot"), TIL = c("-", "-")),
                       class = c("data.table", "data.frame"),
                       row.names = c(NA, -2L))

  specCommon <- structure(list(FILGRUPPE = c("VGS", "VGS"),
                               LESID = c(NA_character_, NA_character_), KOL = c("GEO", "LANDSSB"),
                               TYPE = c(1L, 1L), FRA = c("0", "Tot"), TIL = c("-", "-")),
                       class = c("data.table", "data.frame"),
                       row.names = c(NA, -2L))

  output <- structure(list(AAR = c("2006_2012", "2006_2012"),
                           GEO = c(15L, 15L), KJONN = c(0L, 0L),
                           UTDANN = c(0L, 0L), LANDSSB = c("1C", "2B"),
                           VAL1 = c(187L, 271L)),
                      row.names = c(NA, -2L), class = c("data.table", "data.frame"))

  ## TEST -----------------------------
  expect_equal(is_delete_lesid(dt = data.table::copy(data), code = specRow, lesid = "vers1"), output)
  expect_equal(is_delete_common(dt = data.table::copy(data), code = specCommon, group = "VGS"), output)

})
