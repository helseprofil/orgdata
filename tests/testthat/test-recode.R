test_that("Recode variables", {

  expect_equal(is_recode(dt = recDT, code = recCB, lesid = 16), recOut)
  expect_equal(is_recode_common(dt = recDT, code = recCB), recOut)
})
