
test_that("Manheader return list", {
  manOut <- list(old = c('2', '3'), new = c("KJONN", "ALDER"))

  expect_identical(get_manheader(spec = df01), manOut)
  expect_identical(get_manheader(spec = dfna), NA_character_)
})

test_that("Manheader rename colume by index", {
  # Data ---------------------------------------------------
  spec1 <- structure(list(
    KOBLID = 18L, FILID = 13L, FILGRUPPE = "Dode",
    FILNAVN = "SSB/DODE_SSB/ORG/2021/G42018_v3.xlsx", IBRUKTIL = structure(253370764800, class = c(
      "POSIXct",
      "POSIXt"
    ), tzone = "UTC"), LESID = 17L, FILGRUPPE = "Dode",
    INNLESARG = "ark=Ark1, header=TRUE", EXTRA = NA_character_,
    MANHEADER = NA_character_, GEO = "grunnkrets1", AAR = "aar",
    KJONN = "kjoenn", ALDER = "d_aldh", UTDANN = "utdnivaa",
    LANDSSB = "landb", VAL = "antall"
  ), row.names = 1L, class = "data.frame")


  DT <- structure(list(
    aar = c("2018", "2018"), grunnkrets1 = c(
      "01010102",
      "01010102"
    ), kjoenn = c("1", "1"), d_aldh = c("072", "078"),
    utdnivaa = c("01", "02"), landb = c("0", "0"), sivilstand = c(
      "4",
      "3"
    ), antall = c(1, 1)
  ), row.names = c(NA, -2L), class = c(
    "data.table",
    "data.frame"
  ))

  manspec <- list(old = '5', new = "UTDANN")
  data.table::setDT(DT)
  DT2 <- copy(DT)
  data.table::setnames(DT2, names(DT2)[5], "UTDANN")

  regSpec <- list(old = c("^grun","vaa$"), new = c("GEO", "UTDANN"))
  DT3 <- data.table::copy(DT)
  data.table::setnames(DT3, names(DT)[c(2,5)], c("GEO", "UTDANN"))

  regError1 <- list(old = "^a", new = "AAR")
  regError2 <- list(old = c("nn", "aa"), new = c("GEO", "EDU" ))

  # Test ------------------------------------------------------
  expect_equal(do_manheader(copy(DT), NA_character_), DT)
  expect_equal(do_manheader(copy(DT), manspec), DT2)
  expect_equal(do_manheader(copy(DT), regSpec), DT3)
  expect_error(do_manheader(copy(DT), regError1))
  expect_error(do_manheader(copy(DT), regError2))
})
