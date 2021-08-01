dfna <- data.frame(INNLESARG = NA_character_, MANHEADER = NA_character_)

## INNLESARG -------------------------
test_that("Innlesarg inputs", {
  txt4 <- list(header = TRUE, skip = TRUE, ark = "Sheet1")

  expect_equal(get_innlesarg(df01), txt4)
  expect_identical(get_innlesarg(dfna), NA_character_)
})

## Input from Access ------------------------------
test_that("Output for singel column", {
  expect_identical(get_year(df01), "aar")
  expect_identical(get_column_input(df02, "GEO"), "geo")
  expect_identical(get_column_input(dfna, "MANHEADER"), NA_character_)
})

## Manheader -------------------------
test_that("Manheader return list", {
  manOut <- list(index = c(2L, 3L), col = c("KJONN", "ALDER"))

  expect_identical(get_manheader(df01), manOut)
  expect_identical(get_manheader(dfna), NA_character_)
})
