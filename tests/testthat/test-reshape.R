test_that("Reshape column dummy", {

  ## DATA -------------
  spec <- structure(list(KOBLID = 10L, FILID = 7L, FILGRUPPE = "TEST_DAAR",
                         FILNAVN = "DAAR\\ORG\\2022\\20210528_dar_1990_2020_grunnkrets.csv",
                         IBRUKTIL = structure(2932532, class = "Date"), ID = 6L, LESID = "v1",
                         FILGRUPPE = "TEST_DAAR", INNLESARG = NA_character_, MANHEADER = NA_character_,
                         GEO = "BOKOMMUNE_1_1,GRUNNKRETS_1_1", AAR = "DAAR", KJONN = "KJONN",
                         ALDER = "ALDER_AR", UTDANN = "<0>", LANDSSB = NA_character_,
                         TAB1 = "ARSAK", TAB2 = NA_character_, TAB3 = NA_character_,
                         VAL1 = "ANTALL", VAL2 = NA_character_, VAL3 = NA_character_,
                         RESHAPE_ID = "GEO, AAR, KJONN, ALDER, UTDANN", RESHAPE_VAL = "!(FAAR, BOKOMMUNE_V_DOD, DODSKOMMUNE_K)",
                         RESHAPE_KOL = "TAB1 = ARSAK, VAL1 = ANTALL", EXTRA = NA_character_), row.names = 1L, class = "data.frame")

  vars <- c("AAR", "KJONN", "ALDER", "UTDANN", "LANDSSB", "TAB1", "TAB2",
            "TAB3", "VAL1", "VAL2", "VAL3")


  result <- c("AAR", "KJONN", "ALDER", "UTDANN", "LANDSSB", "TAB2", "TAB3",
              "VAL2", "VAL3")

  ## TEST ------------
  expect_equal(is_reshape_col(vars = vars, spec = spec), result)

})
