
test_that("Check for error for SQL query", {
  fileError <- "test-error.sql"
  sqlError <- paste(readLines(system.file(fileError, package = "orgdata")), collapse = "\n")

  expect_error(check_null(arg = NULL, "Display error"), "Display error")

  expect_error(check_sql(sqlError), "Missing `sprintf` reference in SQL code")
})


test_that("Column output as expected", {
  df <- data.frame(year = "2021 ", head = "TRUE", tail = "FALSE")

  expect_identical(input_type("1", "int"), 1L)
  expect_identical(input_type("1", "double"), 1)
  expect_identical(input_type("1", "char"), "1")

  expect_identical(find_column_input(df, "year", "double"), 2021)
  expect_identical(find_column_input(df, "year", "int"), 2021L)
  expect_identical(find_column_input(df, "year", "char"), "2021")
  expect_identical(find_column_input(df, "head"), TRUE)
  expect_identical(find_column_input(df, "tail"), FALSE)

  expect_true(dummy_input("$Y"))
  expect_false(dummy_input("Y$"))

  expect_error(seperate_value("3"))
  expect_equal(seperate_value("3,5 = age,edu", sep = "="), c("3,5", "age,edu"))
  expect_equal(seperate_value("3,5 = age,edu", sep = "=", keep = 1), "3,5")
  expect_equal(seperate_value("3,5 = age,edu", sep = "=", keep = 2), "age,edu")
})
