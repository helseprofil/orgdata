test_that("Helper functions", {
  txt1 <- "ark=Sheet1"

  expect_equal(is_separate(txt1, "="), c("ark", "Sheet1"))
  expect_equal(is_separate(txt1, "=", 1), "ark")
  expect_equal(is_separate(txt1, "=", 2), "Sheet1")
})

test_that("Get input column", {
  txt1 <- "header=TRUE, ark=Sheet1"
  txt2 <- "header=TRUE : sep=,"
  txt3 <- find_column_multi(txt1)
  txt4 <- list(header = TRUE, ark = "Sheet1")

  expect_equal(find_column_multi(df = df01, col = "INNLESARG", sep = ","), c("header=TRUE", "skip=TRUE", "ark=Sheet1"))
  expect_equal(find_column_multi(df = txt2, sep = ":"), c("header=TRUE", "sep=,"))

  expect_equal(find_column_multi_input(txt3), txt4)

  expect_equal(find_column_multi_input_arg(txt3, "ark"), "Sheet1")
  expect_identical(find_column_multi_input_arg(txt3, "header"), TRUE)
})
