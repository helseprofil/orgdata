
test_that("Read rawdata", {
  expect_error(find_data(file = "raw-data.spss"))
  expect_error(read_file.default(file = "raw-data.spss"))
  expect_error(read_file(file = "raw-data.spss"))
})
