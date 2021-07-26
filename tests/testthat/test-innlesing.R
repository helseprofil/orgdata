test_that("Get input column", {
  txt1 <- "header=TRUE, skip=TRUE"
  txt2 <- "header=TRUE : sep=,"

  expect_equal(get_column_multi_args(txt1), c("header=TRUE", "skip=TRUE"))
  expect_equal(get_column_multi_args(txt2, sep = ":"), c("header=TRUE", "sep=,"))
})
