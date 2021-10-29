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
