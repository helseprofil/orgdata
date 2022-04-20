test_that("Reshape column ID and Value", {

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


  inCol <- list(old = c("variable", "VAL1"), new = c("TAB1", "N"))
  outCol <- list(old = c("variable", "value"), new = c("TAB1", "VAL1"))


  ## TEST ------------
  expect_equal(is_reshape_col(vars = vars, spec = spec), result)
  expect_equal(is_reshape_col_val(inCol), outCol)
})


test_that("Reshape rename column", {

  ## DATA -------------------
  dtt <- structure(list(GEO = c(1234, 1234, 1234, 1234, 1234),
                        AAR = c(1994L, 1997L, 1992L, 1995L, 1994L),
                       KJONN = c("Mann", "Mann", "Kvinne", "Kvinne", "Mann"),
                       ALDER = c(91L, 91L, 90L, 93L, 92L),
                       UTDANN = c("0", "0", "0", "0", "0"),
                       variable = structure(c(24L, 21L, 17L, 17L, 7L),
                                            .Label = c("ALLE_DODSAARSAKER", "HJERTE_OG_KARSYKDOMMER",
                                                       "ISKEMISK_HJERTESYKDOM", "HJERTEINFARKT", "KARSYKDOMMER_I_HJERNEN",
                                                       "HJERNESLAG", "KOLS", "KREFT", "LUNGEKREFT", "KOLS_OG_LUNGEKREFT",
                                                       "PROSTATAKREFT", "BRYSTKREFT", "VOLDSOMME_DODSFALL", "ULYKKER_EUR59",
                                                       "MAGEKREFT", "TYKKTARMSKREFT", "FOFLEKKREFT", "DIABETES", "LUNGESYKDOMMER",
                                                       "KOLS_EMFYSEM_ASTMA_KR_BRONKITT", "FORDOYELSESSYKDOMMER", "KRONISK_LEVERSYKDOM",
                                                       "TRAFIKKULYKKER", "SELVMORD", "NARKOTIKA", "AVHENGIGHET_PSYKISKE_LIDELSER_NARKOTIKA",
                                                       "AVHENGIGHET_OPIOIDER", "RESIDUAL_PSYKOSE", "AVHENGIGHET_FLERE_STOFFER",
                                                       "OPIUM", "OPIUM_OVERDOSE", "OPIUM_SELVMORD", "OPIUM_FORGIFTNING",
                                                       "HEROIN", "HEROIN_OVERDOSE", "HEROIN_SELVMORD", "HEROIN_FORGIFTNING",
                                                       "STIMULANTER_KOKAIN", "KOKAIN_OVERDOSE", "KOKAIN_SELVMORD", "KOKAIN_FORGIFTNING",
                                                       "STIMULANTER_OVERDOSE", "STIMULANTER_SELVMORD", "STIMULANTER_FORGIFTNING",
                                                       "ANDRE_OPIOIDER", "ANDRE_OPIOIDER_OVERDOSE", "ANDRE_OPIOIDER_SELVMORD",
                                                       "ANDRE_OPIOIDER_FORGIFTNING", "METADON", "METADON_OVERDOSE",
                                                       "METADON_SELVMORD", "METADON_FORGIFTNING", "ANDRE_SYNTETISKE_OPIOIDER",
                                                       "ANDRE_SYNTETISKE_OPIOIDER_OVERDOSE", "ANDRE_SYNTETISKE_OPIOIDER_SELVMORD",
                                                       "ANDRE_SYNTETISKE_OPIOIDER_FORGIFTNING", "ANDRE_STOFFER", "ALKOHOL"
                                                       ), class = "factor"),
                       value = c(0L, 0L, 0L, 0L, 0L)),
                  row.names = c(NA, -5L), class = c("data.table", "data.frame"))

  specDTT <- structure(list(KOBLID = 10L, FILID = 7L, FILGRUPPE = "TEST_DAAR",
                            FILNAVN = "DAAR\\ORG\\2022\\20210528_dar_1990_2020_grunnkrets.csv",
                         IBRUKTIL = structure(2932532, class = "Date"), ID = 6L, LESID = "v1",
                         FILGRUPPE = "TEST_DAAR", INNLESARG = NA_character_, MANHEADER = NA_character_,
                         GEO = "BOKOMMUNE_1_1,GRUNNKRETS_1_1", AAR = "DAAR", KJONN = "KJONN",
                         ALDER = "ALDER_AR", UTDANN = "<0>", LANDSSB = NA_character_,
                         TAB1 = "hvorfor", TAB2 = NA_character_, TAB3 = NA_character_,
                         VAL1 = "N", VAL2 = NA_character_, VAL3 = NA_character_, RESHAPE_ID = "GEO, AAR, KJONN, ALDER, UTDANN",
                         RESHAPE_VAL = "-(FAAR, BOKOMMUNE_V_DOD, DODSKOMMUNE_K)",
                         RESHAPE_KOL = "TAB1 = hvorfor, VAL1 = N", EXTRA = NA_character_), row.names = 1L, class = "data.frame")

  outVarId <- list(id = c("GEO", "AAR", "KJONN", "ALDER", "UTDANN"),
                   var = c("variable", "value"),
                   type = "not")

  out <- copy(dtt)
  data.table::setDT(dtt)
  data.table::setnames(out, c("variable", "value"), c("TAB1", "VAL1"))

  #TEST --------------------
  expect_equal(do_reshape_rename_col(dt = copy(dtt), specDTT), out)
  expect_equal(get_reshape_id_val(dt = copy(dtt), spec = specDTT), outVarId)
})


test_that("Reshape long columns", {

  DT <- readRDS(system.file("testdata", "reshape-dt.rds", package = "orgdata"))
  DTout <- readRDS(system.file("testdata", "reshape-out.rds", package = "orgdata"))
  DToutList <- readRDS(system.file("testdata", "reshape-out-list.rds", package = "orgdata"))
  respYes <- readRDS(system.file("testdata", "reshape-respec-correct.rds", package = "orgdata"))
  respList <- readRDS(system.file("testdata", "reshape-respec-list.rds", package = "orgdata"))

  expect_equal(do_reshape(dt = copy(DT), respec = respYes), DTout)
  expect_equal(do_reshape(dt = copy(DT), respec = respList), DToutList)

})
