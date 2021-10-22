test_that("See file", {

  outpt <- list(cyl = structure(list(cyl = c(6, 4, 8),
                                     N = c(7L, 11L, 14L)),
                                row.names = c(NA, -3L),
                                class = c("data.table", "data.frame")),
                gear = structure(list(gear = c(4, 3, 5),
                                      N = c(12L, 15L, 5L)),
                                 row.names = c(NA, -3L),
                                 class = c("data.table", "data.frame")))

  ## TEST
  expect_equal(see_file(mtcars, cyl, gear), outpt)
  expect_equal(see_file(mtcars, "cyl", "gear"), outpt)

})
