test_that("Recode aggregate", {

  ## DATA ---------------
  dt <- structure(list(ALDER = c("73", "28", "51", "58", "61"),
                       AAR = c("1990", "1990", "1990", "1990", "1990"),
                       DIAGNOSE = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
                       UTDANN = c("0", "0", "0", "0", "0")),
                  row.names = c(NA, -5L), class = c("data.table", "data.frame"))

  code <- structure(list(FILGRUPPE = "AGGREGATE", KOL = "DIAGNOSE", TYPE = 1L,
                         FRA = "NA", TIL = "ALLE"),
                    class = c("data.table", "data.frame" ),
                    row.names = c(NA, -1L), index = structure(integer(0), "`__FILGRUPPE`" = integer(0)))

  output <- structure(list(ALDER = c("73", "28", "51", "58", "61"),
                           AAR = c("1990", "1990", "1990", "1990", "1990"),
                           DIAGNOSE = c("ALLE", "ALLE", "ALLE", "ALLE", "ALLE"),
                           UTDANN = c("0", "0", "0", "0", "0")),
                      row.names = c(NA, -5L), class = c("data.table", "data.frame"))

  ## TEST --------------
  expect_equal(is_recode_aggregate(dt, code), output)

})
