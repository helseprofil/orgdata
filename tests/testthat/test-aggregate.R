test_that("Aggregate group list", {

  ## DATA ----------------
  srcCols <- c("kommune", "AAR", "ALDER", "LANDSSB", "UTDANN", "TAB1", "LANDBAK",
               "INNVKAT")

  output <- list(c("kommune", "AAR", "ALDER", "TAB1"),
                 c("kommune", "AAR", "ALDER", "TAB1", "LANDSSB"),
                 c("kommune", "AAR", "ALDER", "TAB1", "UTDANN"),
                 c("kommune", "AAR", "ALDER", "TAB1", "LANDBAK"),
                 c("kommune", "AAR", "ALDER", "TAB1", "INNVKAT"))

  ## TEST --------------
  expect_equal(is_set_list(level = "kommune", srcCols = srcCols), output)


})
