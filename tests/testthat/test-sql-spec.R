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
