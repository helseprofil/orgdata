test_that("Implicit null", {

  ## DATA ---------------------------
  dtimp <- structure(list(GEO = c("01102016", "01072016", "02012016", "01102016",
                                  "01072016", "02022017", "02022017", "02012017", "02022017", "02022017"),
                          AAR = c(2016, 2016, 2016, 2016, 2016, 2017, 2017, 2017, 2017, 2017),
                          v3 = c(rep(c("A", "B"),4), "C", "C"),
                          v4 = c(rep(6:9, 2), 10, 11),
                          v6 = c(3, 4, 3, 1, 3, 2, 4, 2, 4, 2),
                          VAL1 = c(2, 2, 1, 1, 2, 1, 2, 3, 1, 2)),
                     row.names = c(NA, -10L), class = c("data.table", "data.frame"))

  dtOut <- structure(list(GEO = c("99999999", "99999999", "99999999", "99999999",
                                  "99999999", "99999999", "99999999"),
                          AAR = c(2016, 2016, 2016, 2016, 2017, 2017, 2017),
                          v3 = c("C", "A", "A", "A", "A", "A", "A"),
                          v4 = c(6, 10, 11, 6, 6, 6, 6),
                          v6 = c(1, 1, 1, 2, 1, 1, 3),
                          VAL1 = c(0, 0, 0, 0, 0, 0, 0)),
                     row.names = c(NA, -7L), class = c("data.table", "data.frame"))

  dtOutKom <- copy(dtOut)
  dtOutKom[, GEO := gsub("^9999", "", GEO)]
  ## TEST --------------------------

  expect_equal(do_implicit_null(dt = dtimp, level = "grunnkrets"), dtOut)
  expect_equal(do_implicit_null(dt = dtimp, level = "kommune"), dtOutKom)
  expect_error(do_implicit_null(dt = dtimp, level = "town"))
})
