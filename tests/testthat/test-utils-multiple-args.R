

test_that("Get input column", {
  txt1 <- "header=TRUE, ark=Sheet1"
  txt2 <- "header=TRUE : sep=,"
  txt3 <- get_column_multi_args(txt1)

  expect_equal(get_column_multi_args(txt1), c("header=TRUE", "ark=Sheet1"))
  expect_equal(get_column_multi_args(txt2, sep = ":"), c("header=TRUE", "sep=,"))

  expect_equal(get_input_multi_args("ark", txt3), "Sheet1")
  expect_equal(get_input_multi_args("header", txt3), TRUE)
})
