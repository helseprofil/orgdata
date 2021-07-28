
test_that("Check for error for SQL query", {
  fileError <- "test-error.sql"
  sqlError <- paste(readLines(system.file(fileError, package = "orgdata")), collapse = "\n")

  expect_error(check_null(arg = NULL), "Argument for NULL is missing")
  expect_error(check_null(arg = NULL, "Display error"), "Display error")

  expect_error(check_sql(sqlError), "Missing sprintf reference in SQL code")
})


test_that("Column input check", {
  df <- data.frame(year = "2021 ")

  expect_identical(input_type("1", "int"), 1L)
  expect_identical(input_type("1", "double"), 1)
  expect_identical(input_type("1", "char"), "1")

  expect_identical(find_column_input(df, "year", "double"), 2021)
  expect_identical(find_column_input(df, "year", "int"), 2021L)
  expect_identical(find_column_input(df, "year", "char"), "2021")

  expect_true(dummy_input("$Y"))
  expect_false(dummy_input("Y$"))
})
