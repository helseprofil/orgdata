test_that("Connection to DB", {

  geofile <- "F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/STYRING/raw-khelse/geo-koder.accdb"
  khfile <- "F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/STYRING/raw-khelse/raw-database_BE.accdb"

  expect_equal(is_conn_db(db = "geo", .test = TRUE, check = FALSE), geofile)
  expect_equal(is_conn_db(db = "kh", .test = TRUE, check = FALSE), khfile)
  expect_equal(is_conn_db(.test = TRUE, check = FALSE), khfile)

})
