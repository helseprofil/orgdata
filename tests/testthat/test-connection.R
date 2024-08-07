test_that("Connection to DB", {

  geofile <- file.path(os_drive(), "Prosjekt/FHP/PRODUKSJON/STYRING/raw-khelse/geo-koder.accdb" )
  khfile <- file.path(os_drive(), "Prosjekt/FHP/PRODUKSJON/STYRING/raw-khelse/raw-database_BE.accdb" )

  skip_on_ci()
  skip_on_covr()
  skip_on_cran()
  expect_equal(is_conn_db(db = "geo", .test = TRUE, check = FALSE), geofile)
  expect_equal(is_conn_db(db = "kh", .test = TRUE, check = FALSE), khfile)
  expect_equal(is_conn_db(.test = TRUE, check = FALSE), khfile)

})
