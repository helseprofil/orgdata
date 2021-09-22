test_that("Add columns", {
  ## DATA ------------------------------
  dtInn <- structure(list(AAR = c(2019L, 2019L, 2019L), GEO = c(3010202L, 3010206L, 3010206L),
                          KJONN = c(1L, 1L, 1L), ALDER = c(69L, 52L, 67L), UTDANN = c(3L, 2L, 3L),
                          LANDBAK = c("0", "0", "0"), sivil = c(1L, 4L, 4L), VAL = c(1L, 1L, 1L),
                          LANDB = c("0", "0", "0"), LANDF = c(NA_character_, NA_character_, NA_character_)),
                     row.names = c(NA, -3L), class = c("data.table", "data.frame"))

  dtOut <- structure(list(AAR = c(2019L, 2019L, 2019L), GEO = c(3010202L, 3010206L, 3010206L), KJONN = c(1L, 1L, 1L),
                          ALDER = c(69L, 52L, 67L), UTDANN = c(3L, 2L, 3L), LANDBAK = c("0", "0", "0"),
                          SIVILSTAND = c(1L, 4L, 4L), VAL = c(1L, 1L, 1L), LANDB = c("0", "0", "0"),
                          LANDF = c(NA_character_, NA_character_, NA_character_)), row.names = c(NA, -3L),
                     class = c("data.table", "data.frame"))

  output <-  list(old = "sivil", new = "SIVILSTAND")

  fgspec <- structure(list(ID = 10L, FILGRUPPE = "Dode", AGGREGERE = "F,K",
                           KOLNAVN = "sivil = SIVILSTAND", ADDVAL = NA_character_,
                           NAVAL = NA_character_, SPLITTFRA = "LANDBAK", SPLITTTIL = "LANDB, LANDF"),
                      class = "data.frame", row.names = c(NA, -1L))
  ## TEST ------------------
  expect_equal(get_colname(spec = fgspec), output)
  expect_equal(do_colname(dtInn, output), dtOut )

})
