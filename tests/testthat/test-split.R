test_that("Get split input", {
  fsplit <- list(from = "LANDBAK", to = c("landb", "landf"))
  data.table::setDT(DT01)

  expect_equal(get_split(spec = befolk), fsplit)
  expect_error(get_split())

  expect_equal(do_split(DT01, fsplit), data.table::setDT(DFout))
  expect_error(do_split(DT01, data.table::setDF(fsplit)))
})
