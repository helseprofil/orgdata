test_that("Aggregate group list", {

  ## DATA ----------------
  srcCols <- c("kommune", "AAR", "ALDER", "LANDSSB", "UTDANN", "TAB1", "LANDBAK",
               "INNVKAT")

  output <- list(c("kommune", "AAR", "ALDER", "TAB1", "LANDSSB"),
                 c("kommune", "AAR", "ALDER", "TAB1", "UTDANN"),
                 c("kommune", "AAR", "ALDER", "TAB1", "LANDBAK"),
                 c("kommune", "AAR", "ALDER", "TAB1", "INNVKAT"),
                 c("kommune", "AAR", "ALDER", "TAB1", "LANDSSB", "UTDANN"),
                 c("kommune", "AAR", "ALDER", "TAB1", "LANDSSB", "LANDBAK"),
                 c("kommune", "AAR", "ALDER", "TAB1", "LANDSSB", "INNVKAT"),
                 c("kommune", "AAR", "ALDER", "TAB1", "UTDANN", "LANDBAK"),
                 c("kommune", "AAR", "ALDER", "TAB1", "UTDANN", "INNVKAT"),
                 c("kommune", "AAR", "ALDER", "TAB1", "LANDBAK", "INNVKAT"),
                 c("kommune", "AAR", "ALDER", "TAB1", "LANDSSB", "UTDANN", "LANDBAK"),
                 c("kommune", "AAR", "ALDER", "TAB1", "LANDSSB", "UTDANN", "INNVKAT"),
                 c("kommune", "AAR", "ALDER", "TAB1", "LANDSSB", "LANDBAK", "INNVKAT"),
                 c("kommune", "AAR", "ALDER", "TAB1", "UTDANN", "LANDBAK", "INNVKAT"),
                 c("kommune", "AAR", "ALDER", "TAB1", "LANDSSB", "UTDANN", "LANDBAK", "INNVKAT"),
                 c("kommune", "AAR", "ALDER", "TAB1"))
  ## TEST --------------
    expect_equal(is_set_list(level = "kommune", srcCols = srcCols), output)


})


test_that("Aggregate group list with AGGKOL", {


  ## DATA ------------
  level <- "bydel"
  srcCols <- c("bydel", "AAR", "KJONN", "ALDER", "UTDANN", "LANDBAK", "INNVKAT")
  colx <- "KJONN"

  output <- list(c("bydel", "AAR", "ALDER", "KJONN"),
                 c("bydel", "AAR", "ALDER", "UTDANN"),
                 c("bydel", "AAR", "ALDER", "LANDBAK"),
                 c("bydel", "AAR", "ALDER", "INNVKAT"),
                 c("bydel", "AAR", "ALDER", "KJONN", "UTDANN"),
                 c("bydel", "AAR", "ALDER", "KJONN", "LANDBAK"),
                 c("bydel", "AAR", "ALDER", "KJONN", "INNVKAT"),
                 c("bydel", "AAR", "ALDER", "UTDANN", "LANDBAK"),
                 c("bydel", "AAR", "ALDER", "UTDANN", "INNVKAT"),
                 c("bydel", "AAR", "ALDER", "LANDBAK", "INNVKAT"),
                 c("bydel", "AAR", "ALDER", "KJONN", "UTDANN", "LANDBAK"),
                 c("bydel", "AAR", "ALDER", "KJONN", "UTDANN", "INNVKAT"),
                 c("bydel", "AAR", "ALDER", "KJONN", "LANDBAK", "INNVKAT"),
                 c("bydel", "AAR", "ALDER", "UTDANN", "LANDBAK", "INNVKAT"),
                 c("bydel", "AAR", "ALDER", "KJONN", "UTDANN", "LANDBAK", "INNVKAT"),
                 c("bydel", "AAR", "ALDER"))
  ##TEST -------------
  expect_equal(is_set_list(level = level, srcCols = srcCols, colx = colx), output)


})
