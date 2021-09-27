test_that("Mutate column", {
  ## Data -------------------------

  dt <- structure(list(GEO = c("102", "102", "102", "102", "102"),
                       KJONN = c(1L, 1L, 1L, 1L, 1L), ALDER = c(0L, 1L, 4L, 6L, 18L),
                       LANDSSB = c("1C", "0", "1C", "3B", "0"), VAL1 = c(1L, 1L, 1L, 1L, 1L),
                       AAR = c(2021L, 2021L, 2021L, 2021L, 2021L)),
                  row.names = c(NA, -5L), class = c("data.table", "data.frame"))


  spec <- structure(list(KOBLID = 2L, FILID = 1L, FILGRUPPE = "TEST01",
                         IBRUKTIL = structure(2932532, class = "Date"),
                         ID = 1L, LESID = "v01", FILGRUPPE = "TEST01", INNLESARG = NA_character_,
                         MANHEADER = NA_character_, GEO = "grunnkrets", AAR = "$Y",
                         KJONN = "kjoenn", ALDER = "alderu", UTDANN = "<1>", LANDSSB = "landb",
                         TAB1 = NA_character_, TAB2 = NA_character_, TAB3 = NA_character_,
                         VAL1 = "antall", VAL2 = NA_character_, VAL3 = NA_character_,
                         EXTRA = NA_character_), row.names = 1L, class = "data.frame")


  output <- structure(list(GEO = c("102", "102", "102", "102", "102"),
                           KJONN = c(1L, 1L, 1L, 1L, 1L), ALDER = c(0L, 1L, 4L, 6L, 18L),
                           LANDSSB = c("1C", "0", "1C", "3B", "0"), VAL1 = c(1L, 1L, 1L, 1L, 1L),
                           AAR = c(2021L, 2021L, 2021L, 2021L, 2021L),
                           UTDANN = c("1", "1", "1", "1", "1" )),
                      row.names = c(NA, -5L), class = c("data.table", "data.frame"))

  ## TEST ---------------
  expect_equal(do_mutate(dt = dt, spec = spec), output)
})
