test_that("Check missing arg and SQL error", {
  ## File is in inst/ folder
  file <- "test.sql"
  fileError <- "test-error.sql"
  value <- 9

  expect_error(is_query(), "Argument for `file` is missing")
  ## expect_error(find_query(file), "Argument for `value` is missing")
  expect_error(is_query(fileError, value), "Missing `sprintf` reference in SQL code")

  expect_error(find_spec(file, value), "Argument for `con` is missing")
})


test_that("SQL query", {

  sqQry <- "select FILGRUPPE, LESID, KOL, TYPE, FRA, TIL\nfrom tbl_Kode\nwhere (FILGRUPPE = 'TEST' or FILGRUPPE = 'ALLE')\n  and VERSJONTIL = #9999-01-01#\n  and (LESID is NULL or LESID = 'ver01')\n  and TYPE = 'AG'\n  and TIL <> '-'"
  sqOpp <- "SELECT * FROM tblGeo\nWHERE validTo='1990' AND level='GRK'"


  ## TEST
  expect_equal(is_query(file = "recode-aggregate.sql", char = "TEST", char2 = "ver01"), sqQry)
  expect_equal(is_query(file = "geo-code.sql", char = "GRK", char2 = 1990, opposite = TRUE), sqOpp)
})
