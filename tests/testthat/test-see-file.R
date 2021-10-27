test_that("See file", {

  outpt <- list(cyl = structure(list(cyl = c(4, 6, 8),
                                     N = c(11L, 7L, 14L)),
                                row.names = c(NA, -3L),
                                class = c("data.table", "data.frame"),
                                sorted = "cyl"),
                gear = structure(list(gear = c(3, 4, 5),
                                      N = c(15L, 12L, 5L)),
                                 row.names = c(NA, -3L),
                                 class = c("data.table", "data.frame"),
                                 sorted = "gear"))

  dtc <- mtcars[, c("vs", "am", "gear", "carb")]

  dtout <- list(vs = structure(list(vs = c(0, 1), N = c(18L, 14L)),
                               row.names = c(NA, -2L),
                               class = c("data.table", "data.frame"),
                               sorted = "vs"),
                am = structure(list(am = c(0, 1), N = c(19L, 13L)),
                               row.names = c(NA, -2L),
                               class = c("data.table", "data.frame"), sorted = "am"),
                gear = structure(list(gear = c(3, 4, 5), N = c(15L, 12L, 5L)),
                                 row.names = c(NA, -3L),
                                 class = c("data.table", "data.frame"),
                                 sorted = "gear"),
                carb = structure(list(carb = c(1, 2, 3, 4, 6, 8),
                                      N = c(7L, 10L, 3L, 10L, 1L, 1L)),
                                 row.names = c(NA, -6L),
                                 class = c("data.table", "data.frame"),
                                 sorted = "carb"))

  ## TEST
  expect_equal(see_file(dtc), dtout)
  expect_equal(see_file(mtcars, cyl, gear), outpt)
  expect_equal(see_file(mtcars, "cyl", "gear"), outpt)
  expect_equal(see_file(mtcars, c(2,10)), outpt)
  expect_error(see_file(letters[1:5]))
})
