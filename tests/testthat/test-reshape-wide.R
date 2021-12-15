test_that("Reshape wide", {

  ## DATA ---
  spec <- list(rescol = "TAB1", resval = "VAL1", valcols = c("2", "1"))
  dt <- structure(list(AAR = c(2007L, 2007L),
                       GEO = c(11510101L, 11510101L),
                       KJONN = c(1L, 1L), UTDANN = c(2L, 2L), LANDSSB = c(0L, 0L),
                       TAB1 = 2:1, TAB2 = c(5L, 5L), VAL1 = c(1L, 1L),
                       VAL2 = c(2L, 2L), LANDBAK = c("0", "0"), INNVKAT = c("0", "0")),
                  row.names = c(NA, -2L), class = c("data.table", "data.frame"))
  output <- structure(list(AAR = 2007L, GEO = 11510101L, KJONN = 1L, UTDANN = 2L,
                           LANDSSB = 0L, TAB2 = 5L, VAL2 = 2L, LANDBAK = "0", INNVKAT = "0",
                           `1` = 1L, `2` = 1L),
                      row.names = c(NA, -1L), class = c("data.table", "data.frame"),
                      sorted = c("AAR", "GEO", "KJONN", "UTDANN", "LANDSSB", "TAB2",
                                 "VAL2", "LANDBAK", "INNVKAT"))
  ## TEST ----
  expect_equal(do_reshape_wide(dt = dt, respec = spec), output)

})
