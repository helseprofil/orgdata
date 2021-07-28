

test_that("Get input column", {
  txt1 <- "header=TRUE, ark=Sheet1"
  txt2 <- "header=TRUE : sep=,"
  txt3 <- find_column_multi(txt1)

  expect_equal(find_column_multi(txt1), c("header=TRUE", "ark=Sheet1"))
  expect_equal(find_column_multi(txt2, sep = ":"), c("header=TRUE", "sep=,"))

  expect_equal(find_column_multi_input("ark", txt3), "Sheet1")
  expect_equal(find_column_multi_input("header", txt3), TRUE)
})
