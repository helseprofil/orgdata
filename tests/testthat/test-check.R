
test_that("Check for error for SQL query", {
  ## File is in inst/ folder
  file <- "test.sql"
  fileError <- "test-error.sql"
  sqlError <- paste(readLines(system.file(fileError, package = "orgdata")), collapse = "\n")
  value <- 9

  expect_error(check_null(arg = NULL), "Argument for NULL is missing")
  expect_error(check_null(arg = NULL, "Display error"), "Display error")

  expect_error(check_sql(sqlError), "Missing sprintf reference in SQL code")

  expect_error(get_query(), "Argument for file is missing")
  expect_error(get_query(file), "Argument for value is missing")
  expect_error(get_query(fileError, value), "Missing sprintf reference in SQL code")

  expect_error(read_spec(file, value), "Argument for con is missing")
})
