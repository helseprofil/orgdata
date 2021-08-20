
test_that("Check for error for SQL query", {
  fileError <- "test-error.sql"
  sqlError <- paste(readLines(system.file(fileError, package = "orgdata")), collapse = "\n")

  expect_error(is_null(arg = NULL, "Display error"), "Display error")
  expect_error(is_null_both(x = NULL, y = NULL))
  expect_identical(is_null_both(x = "NotNull", y = NULL), NULL)
  expect_identical(is_null_both(y = "NotNull"), NULL)

  expect_error(is_not_null_both("A", "B"))
  expect_identical(is_not_null_both(x = "NotNull"), NULL)
  expect_identical(is_not_null_both(), NULL)

  expect_error(is_sql_code(sqlError), "Missing `sprintf` reference in SQL code")
})


test_that("Column output as expected", {
  df <- data.frame(year = "2021 ", head = "TRUE", tail = "FALSE", txt = "Text")

  expect_identical(is_input_type("1", "int"), 1L)
  expect_identical(is_input_type("1", "double"), 1)
  expect_identical(is_input_type("1", "char"), "1")

  expect_identical(find_column_input(df, "year", "double"), 2021)
  expect_identical(find_column_input(df, "year", "int"), 2021L)
  expect_identical(find_column_input(df, "year", "char"), "2021")
  expect_identical(find_column_input(df, "txt"), "Text")
  expect_identical(find_column_input(df, "head"), TRUE)
  expect_identical(find_column_input(df, "tail"), FALSE)

  expect_true(is_dummy("$Y"))
  expect_false(is_dummy("Y$"))

  expect_error(is_separate("3"))
  expect_equal(is_separate("3,5 = age,edu", sep = "="), c("3,5", "age,edu"))
  expect_equal(is_separate("3,5 = age,edu", sep = "=", keep = 1), "3,5")
  expect_equal(is_separate("3,5 = age,edu", sep = "=", keep = 2), "age,edu")
})

test_that("Verbose message", {
  op <- options()
  options(orgdata.verbose = TRUE)
  expect_message(is_verbose("C:/File/Name", "File:"), "File: C:/File/Name")
  expect_message(is_verbose("C:/File/Name"), " C:/File/Name")
  options(op)
})
