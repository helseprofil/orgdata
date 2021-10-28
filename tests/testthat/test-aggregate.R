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


test_that("Get aggregate levels", {

  #DATA -----------------------
  spec <-structure(list(ID = 1L, FILGRUPPE = "TEST01", UTMAPPE = "SSB\\BEFOLKNING\\ORG\\2023",
                        AGGREGERE = "B,F", AGGKOL = "KJONN", KOLNAVN = "VAL1=ANTALL",
                        SPLITTFRA = "LANDSSB", SPLITTTIL = "LANDBAK, INNVKAT"),
                   class = "data.frame", row.names = c(NA, -1L))

  output <- c("bydel", "fylke")

  ## TEST -------------
  expect_equal(get_aggregate(spec = spec), output)


})


test_that("Aggegate deprecated functions", {

  expect_error(do_aggregate_recode_standard(mtcars))
  expect_error(is_aggregate_standard_cols())
  expect_error(is_aggregate_recode(mtcars, "gear", "gear2"))
  expect_error(is_active())

})

## test_that("Geo level NA", {

##   dtf <- ata.table::data.table(fylke = c(12, NA), VAL = 1:2)
##   dtk <- data.table::data.table(kommune = c(1234, NA), VAL = 1:2)
##   dtb <- data.table::data.table(bydel = c(123456, NA), VAL = 1:2)

##   dtfo <-

##     ## TEST ------------
##     expect_equal(is_level_na(dtf, "fylke"))


## })
