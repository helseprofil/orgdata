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

  ## Test ---------
  expect_equal(is_grunnkrets_99(dt), dtout)
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

  ## test --------
  expect_equal(is_unknown_grunnkrets(dt = dt, kom = kom), dtout)
})
