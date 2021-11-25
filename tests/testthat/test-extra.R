test_that("Delete NA rows", {

  ## Data ----------------
  dd <- data.table::data.table(v1 = c(1:5,NA), v2 = c(letters[1:5], NA))

  dout <- structure(list(v1 = 1:5, v2 = c("a", "b", "c", "d", "e")),
                    row.names = c(NA, -5L), class = c("data.table", "data.frame"))

  ## Test ----------------
  expect_equal(is_delete_na_row(dd, "DeleteNaRow"), dout)

})
