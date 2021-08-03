test_that("Rename standard columns", {
  dfCol <- data.frame(
    GEO = "grunnkrets", AAR = "$Y", KJONN = "kjoenn",
    ALDER = "age", UTDANN = NA_character_, LANDBAK = "landb", VAL = "total"
  )

  output <- list(
    old = c("grunnkrets", "kjoenn", "age", "landb", "total"),
    new = c("GEO", "KJONN", "ALDER", "LANDBAK", "VAL")
  )

  # Test -------------------------------------
  expect_equal(is_column_name(dfCol, "AAR"), list(col = "AAR", input = NA))
  expect_equal(is_column_name(dfCol, "ALDER"), list(col = "ALDER", input = "age"))
  expect_equal(is_column_name(dfCol, "UTDANN"), list(col = "UTDANN", input = NA_character_))
  expect_equal(is_column_standard(dfCol), output)
})
