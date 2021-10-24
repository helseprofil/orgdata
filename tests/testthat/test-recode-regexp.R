test_that("Regular expression", {

  ## DATA ---------------
  dt01 <- structure(list(AAR = c(2020L, 2020L, 2020L),
                         GEO = c("0301xxxx", "1001xxxx", "1101xxxx"),
                       VAL1 = c(25L, 1L, 1L)), row.names = c(NA, -3L),
                  class = c("data.table", "data.frame"))

  code01 <- structure(list(FILGRUPPE = "FODTE", LESID = "vers1", KOL = "GEO",
                           TYPE = "RE", FRA = "rex(n_times(\"x\", 4), end)", TIL = "9999"),
                    class = c("data.table", "data.frame"), row.names = c(NA, -1L))

  code02 <- structure(list(FILGRUPPE = "FODTE", LESID = NA, KOL = "GEO",
                           TYPE = "RE", FRA = "rex(n_times(\"x\", 4), end)", TIL = "9999"),
                      class = c("data.table", "data.frame"), row.names = c(NA, -1L))

  code03 <- structure(list(FILGRUPPE = "ALLE", LESID = NA, KOL = "GEO",
                           TYPE = "RE", FRA = "rex(n_times(\"x\", 4), end)", TIL = "9999"),
                      class = c("data.table", "data.frame"), row.names = c(NA, -1L))

  output01 <- structure(list(AAR = c(2020L, 2020L, 2020L),
                             GEO = c("03019999", "10019999", "11019999"),
                             VAL1 = c(25L, 1L, 1L)),
                        row.names = c(NA, -3L), class = c("data.table", "data.frame"))


  ## TEST ------------
  expect_equal(is_recode_lesid_regexp(dt = dt01, code = code01, lesid = "vers1"), output01)
  expect_equal(is_recode_common_regexp(dt = dt01, code = code02, group = "FODTE"), output01)
  expect_equal(is_recode_all_regexp(dt = dt01, code = code03), output01)
})
