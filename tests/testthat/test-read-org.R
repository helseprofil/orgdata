test_that("Find DB file or rawdata file", {

})

test_that("Select only valid files", {

  # Data ---------------------------------------------------
  specDT <- structure(list(KOBLID = c(18L, 14L), FILID = c(13L, 5L), MANHEADER = c(
    NA,
    "5=UTDANN"
  ), IBRUKTIL = structure(c(2932532, 10957), class = "Date")), class = c(
    "data.table",
    "data.frame"
  ), row.names = c(NA, -2L))


  brukDT <- structure(list(
    KOBLID = 18L, FILID = 13L, MANHEADER = NA_character_,
    IBRUKTIL = structure(2932532, class = "Date")
  ), class = c(
    "data.table",
    "data.frame"
  ), row.names = c(NA, -1L))

  oldDT <- structure(list(
    KOBLID = 14L, FILID = 5L, MANHEADER = "5=UTDANN",
    IBRUKTIL = structure(10957, class = "Date")
  ), class = c(
    "data.table",
    "data.frame"
  ), row.names = c(NA, -1L))

  expect_equal(is_org_files(specDT, id = NULL), data.table::setDF(brukDT))
  expect_equal(is_org_files(specDT, 18), data.table::setDF(brukDT))
  expect_error(is_org_files(specDT, 14), "No valid file to be processed!")
})
