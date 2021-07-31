## INNLESARG -------------------------
test_that("Innlesarg inputs", {
  txt4 <- list(header = TRUE, skip = TRUE, ark = "Sheet1")

  expect_equal(get_innlesarg(df01), txt4)
})

## Input from Access ------------------------------
test_that("Output for singel column", {
  expect_identical(get_year(df01), "aar")
  expect_identical(get_column_input(df02, "GEO"), "geo")
})

## Manheader -------------------------
test_that("Manheader return list", {
  manOut <- list(index = c("2", "3"), col = c("KJONN", "ALDER"))

  expect_equal(get_manheader(df01), manOut)
})
