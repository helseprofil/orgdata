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
