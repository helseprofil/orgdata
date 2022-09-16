test_that("Add grunnkrets code", {
  ## DATA --------

  dgrk <- structure(list(AAR = c(1990L, 1990L), GEO = c(815L, 1129L),
                         VAL1 = c(1L, 1L)), row.names = c(NA, -2L),
                  class = c("data.frame"))

  out <- structure(list(AAR = c(1990L, 1990L), GEO = c(8159999L, 11299999L),
                        VAL1 = c(1L, 1L)), row.names = c(NA, -2L),
                   class = c("data.frame"))

  data.table::setDT(dgrk)
  data.table::setDT(out)

  ## TEST ----------
  expect_equal(is_grunnkrets(dgrk), out)

})


test_that("Grunnkrets ends with 00", {

  ## Data ---------
  input <- structure(list(code = c("03010100", "03010101", "54449900", "54449999"),
                          name = c("Sentrum 1", "Sentrum 1  - Rode 1", "Uoppgitt delområde", "Uoppgitt grunnkrets"),
                          validTo = c("2021", "2021", "2021", "2021"),
                          level = c("grunnkrets", "grunnkrets", "grunnkrets", "grunnkrets"),
                          grunnkrets = c("03010100", "03010101", "54449900", "54449999"),
                          kommune = c(NA, "0301", NA, "5444"), fylke = c(NA, "03", NA, "54"),
                          bydel = c(NA, "030116", NA, NA)),
                     row.names = c(NA, -4L),
                     class = c("data.table", "data.frame"), sorted = "code")

  output <- structure(list(code = c("03010100", "03010101", "54449900", "54449999"),
                           name = c("Sentrum 1", "Sentrum 1  - Rode 1", "Uoppgitt delområde", "Uoppgitt grunnkrets"),
                           validTo = c("2021", "2021", "2021", "2021"),
                           level = c("grunnkrets", "grunnkrets", "grunnkrets", "grunnkrets"),
                           grunnkrets = c("03010100", "03010101", "54449900", "54449999"),
                           kommune = c("0301", "0301", "5444", "5444"),
                           fylke = c("03", "03", "54", "54"),
                           bydel = c("030116", "030116", NA, NA)),
                      row.names = c(NA, -4L),
                      class = c("data.table", "data.frame"), sorted = "code")

  ## Test
  expect_equal(is_grunnkrets_00(input), output)

})

test_that("Grunnkrets lack 99999999 code", {

  ## Data ------------

  dt <- structure(list(oldCode = c("20300505", NA, NA),
                       oldName = c("Øvre pasvik", NA, NA),
                       currentCode = c("54440505", "54449900", "54449999"),
                       newName = c("Øvre Pasvik", "Uoppgitt delområde", "Uoppgitt grunnkrets"),
                       changeOccurred = c("2020", "2020", "2020")),
                  row.names = c(NA, -3L),
                  class = c("data.table", "data.frame"), sorted = "currentCode")

  dtout <- structure(list(oldCode = c("20300505", NA, NA, "99999999"),
                          oldName = c("Øvre pasvik", NA, NA, "Uoppgitt"),
                          currentCode = c("54440505", "54449900", "54449999", "99999999"),
                          newName = c("Øvre Pasvik", "Uoppgitt delområde", "Uoppgitt grunnkrets", "Uoppgitt"),
                          changeOccurred = c("2020", "2020", "2020", "2020")),
                     row.names = c(NA, -4L),
                     class = c("data.table", "data.frame"))

  dtt <- readRDS(file.path( system.file(package = "orgdata"),"testdata", "GeoUknown99.rds" ))
  dttOut <- readRDS(file.path( system.file (package = "orgdata" ), "testdata", "GeoUknown99out.rds"))

  ## Test ---------
  expect_equal(is_geo_99(dt, type = "grunnkrets"), dtout)
  expect_equal(is_geo_99(dtt, type = "kommune"), dttOut)
})


test_that("Unknown grunnkrets from municipality", {

  ## Data -----------

  dt <- structure(list(oldCode = c(NA_character_, NA_character_, NA_character_),
                       oldName = c(NA_character_, NA_character_, NA_character_),
                       currentCode = c("11030100", "11030101", "11030102"),
                       newName = c("Buøy", "Buøy 2", "Buøy 1"),
                       changeOccurred = c("2020", "2020", "2020")),
                  row.names = c(NA, -3L),
                  class = c("data.table", "data.frame"), sorted = c("currentCode", "changeOccurred"))


  kom <- structure(list(oldCode = c(NA, "1141", "1142", NA),
                        oldName = c(NA, "Finnøy", "Rennesøy", NA),
                        currentCode = c("1101", "1103", "1103", "1106"),
                        newName = c("Eigersund", "Stavanger", "Stavanger", "Haugesund"),
                        changeOccurred = c("2020", "2020", "2020", "2020")),
                   row.names = c(NA, -4L), class = c("data.table", "data.frame"),
                   sorted = c("currentCode", "changeOccurred"))

  dtout <- structure(list(oldCode = c(NA, NA, NA, "11419999", "11429999"),
                          oldName = c(NA, NA, NA, "Finnøy", "Rennesøy"),
                          currentCode = c("11030100", "11030101", "11030102", "11039999", "11039999"),
                          newName = c("Buøy", "Buøy 2", "Buøy 1", "Stavanger", "Stavanger"),
                          changeOccurred = c("2020", "2020", "2020", "2020", "2020")),
                     row.names = c(NA, -5L), class = c("data.table", "data.frame"), sorted = "currentCode")

  geoDT <- readRDS(system.file("testdata", "GeoLevel99.rds", package = "orgdata"))
  geoKom <- readRDS(system.file("testdata", "GeoLevel99kom.rds", package = "orgdata"))
  geoOut <- readRDS(system.file("testdata", "GeoLevel99out.rds", package = "orgdata"))

  ## test --------
  expect_equal(is_unknown_geo(dt = dt, dd = kom, type = "grunnkrets"), dtout)
  expect_equal(is_unknown_geo(dt = geoDT, dd = geoKom, type = "kommune"), geoOut)
})


test_that("Grunnkrets with 0000", {

  ## DATA -----------
  dtraw <- structure(list(AAR = c(2020L, 2020L, 2020L, 2020L, 2020L),
                          GEO = c(54260000L, 55442233L, 54260000L, 54260000L, 54260000L),
                          ALDER = c(0L, 1L, 2L, 3L, 3L),
                          TAB1 = c("eier", "eier", "eier", "eier", "leier"),
                          VAL1 = c(4L, 2L, 2L, 2L, 2L), VAL2 = c(4L, 2L, 2L, 4L, 4L),
                          origin = c(54260000L, 55442233L, 54260000L, 54260000L, 54260000L),
                          dummy_grk = c(0, 0, 0, 0, 0)), row.names = c(NA, -5L),
                     class = c("data.table", "data.frame"))

  dtout <- structure(list(AAR = c(2020L, 2020L, 2020L, 2020L, 2020L),
                          GEO = c(54269999L, 55442233L, 54269999L, 54269999L, 54269999L),
                          ALDER = c(0L, 1L, 2L, 3L, 3L),
                          TAB1 = c("eier", "eier", "eier", "eier", "leier"),
                          VAL1 = c(4L, 2L, 2L, 2L, 2L), VAL2 = c(4L, 2L, 2L, 4L, 4L),
                          origin = c(54260000L, 55442233L, 54260000L, 54260000L, 54260000L),
                          dummy_grk = c(0, 0, 0, 0, 0)),
                     row.names = c(NA, -5L), class = c("data.table", "data.frame"))

  ## TEST
  expect_equal(is_grunnkrets_0000(dtraw), dtout)

})


test_that("Recode geo", {

  #DATA
  code <- structure(list(GEO = c(50510310L, 50510311L, 50510312L, 50519999L, 99999999L),
                         to = c(50600310L, 50600311L, 50600312L, 50609999L, 99999999L),
                         changeOccurred = c(2020L, 2020L, 2020L, 2020L, 2021L),
                         batch = structure(c(1638748800, 1638748800, 1638748800, 1638748800, 1638748800),
                                           class = c("POSIXct", "POSIXt"), tzone = "UTC")),
                    class = c("data.table", "data.frame"), row.names = c(NA, -5L),
                    sorted = c("GEO", "to"))

  dt <- structure(list(AAR = c("2017", "2017", "2017", "2017", "2017"),
                       GEO = c(50510310L, 50510311L, 50510312L, 50519999L, 99999999L),
                       ALDER = c("          18", "          22", "          23", "          24", "          25"),
                       VAL1 = c(1L, 2L, 1L, 2L, 4L)),
                  row.names = c(NA, -5L), class = c("data.table", "data.frame"))

  dtout <- structure(list(AAR = c("2017", "2017", "2017", "2017", "2017"),
                          GEO = c(50600310L, 50600311L, 50600312L, 50609999L, 99999999L),
                          ALDER = c("          18", "          22", "          23", "          24", "          25"),
                          VAL1 = c(1L, 2L, 1L, 2L, 4L),
                          origin = c(50510310L, 50510311L, 50510312L, 50519999L, 99999999L),
                          dummy_grk = c(0, 0, 0, 0, 0)),
                     row.names = c(NA, -5L), class = c("data.table", "data.frame"))

  ## TEST
  expect_equal(do_geo_recode(dt = dt,
                             code = code,
                             type = "grunnkrets",
                             year = 2021,
                             geo = FALSE,
                             base = FALSE,
                             control = TRUE), dtout)
})


