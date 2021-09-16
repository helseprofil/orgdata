test_that("Recode variables", {

  dtt <- structure(list(VAL = c(1L, 1L), LANDB = c("1", "0"), LANDF = c("B", NA)),
                   row.names = c(NA, -2L), class = c("data.table", "data.frame"))

  dtout <- structure(list(VAL = c(1L, 1L), LANDB = c("1", "0"), LANDF = c("B", "NA")),
                     row.names = c(NA, -2L), class = c("data.table", "data.frame"))

  code <- structure(list(KOL = c("LANDF", "LANDF", "LANDF"),
                         FRA = c("B", "C", "NA"),
                         TIL = c("2", "3", "1")),
                    row.names = c(NA, -3L), class = c("data.table", "data.frame"))

  expect_equal(is_recode_lesid(dt = recDT, code = recCB, lesid = 16), recOut)
  expect_equal(is_recode_common(dt = recDT, code = recCB), recOut)
  expect_equal(is_NA(dt = dtt, code = code, col = "LANDF"), dtout)
})
