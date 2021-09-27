test_that("Rename standard columns", {
  dfCol <- data.frame(
    GEO = "grunnkrets", AAR = "$Y", KJONN = "kjoenn",
    ALDER = "age", UTDANN = NA_character_, LANDSSB = "landb", VAL1 = "total"
  )

  output <- list(
    old = c("grunnkrets", "kjoenn", "age", "landb", "total"),
    new = c("GEO", "KJONN", "ALDER", "LANDSSB", "VAL1")
  )


  rawdt <- structure(list(grunnkrets = c("0806", "210"), kjoenn = 1:2, age = c(
    74L, 23L
  ), landb = c("0", "0"), total = c(2L, 23L)),
  row.names = c(NA, -2L),
  class = c("data.table", "data.frame")
  )


  rawOut <- structure(list(GEO = c("0806", "210"), KJONN = 1:2, ALDER = c(
    74L,
    23L
  ), LANDSSB = c("0", "0"), VAL1 = c(2L, 23L)), row.names = c(
    NA,
    -2L
  ), class = c("data.table", "data.frame"))

  # Test -------------------------------------
  expect_equal(is_column_na(dfCol, "AAR"), list(col = "AAR", input = NA))
  expect_equal(is_column_na(dfCol, "ALDER"), list(col = "ALDER", input = "age"))
  expect_equal(is_column_na(dfCol, "UTDANN"), list(col = "UTDANN", input = NA_character_))

  expect_equal(is_check_cols(letters[1:3], letters[1:3]), NULL)
  expect_error(is_check_cols(letters[1:4], letters[1:3]), "Can't find column: d")

  expect_equal(get_column_standard(spec = dfCol), output)

  expect_equal(do_column_standard(dt = rawdt, spec = output), data.table::setDT(rawOut))
})
