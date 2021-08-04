test_that("Year either dummy or not", {
  dt1 <- data.table::data.table(GEO = 210, ALDER = 22)
  dtOut <- data.table::data.table(GEO = 210, ALDER = 22, AAR = 2021L)

  yr <- list(year = 2021L, dummy = TRUE)

  ## Test ----------------------------
  expect_equal(get_year(df01), list(year = "aar", dummy = FALSE))
  expect_equal(do_year(dt = dt1, year = yr), dtOut)
  expect_equal(do_year(dt = dt1, year = yr), dt1)
})
