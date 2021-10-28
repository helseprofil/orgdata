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



test_that("Geo level NA", {
  dt <- structure(list(GEO = c(3010101L, 3010101L, 3010101L, 3010101L, 3010102L),
                       UTDANN = c(1L, 1L, 3L, 2L, 3L), VAL2 = c(1L, 1L, 1L, 1L, 1L),
                       grunnkrets = c(3010101L, 3010101L, 3010101L, 3010101L, 3010102L),
                       kommune = c(NA, 301L, 301L, 301L, 301L),
                       fylke = c(3L, 3L, NA, 3L, 3L),
                       bydel = c(30116L, 30116L, 30116L, 30116L, NA)),
                  row.names = c(NA, -5L), class = c("data.table", "data.frame"))

  dtoutk <- structure(list(GEO = c(3010101L, 3010101L, 3010101L, 3010101L, 3010102L),
                           UTDANN = c(1L, 1L, 3L, 2L, 3L), VAL2 = c(1L, 1L, 1L, 1L, 1L),
                           grunnkrets = c(3010101L, 3010101L, 3010101L, 3010101L, 3010102L),
                           kommune = c(301L, 301L, 301L, 301L, 301L),
                           fylke = c(3L, 3L, NA, 3L, 3L),
                           bydel = c(30116L, 30116L, 30116L, 30116L, NA)),
                      row.names = c(NA, -5L), class = c("data.table", "data.frame"))

  dtoutf <- structure(list(GEO = c(3010101L, 3010101L, 3010101L, 3010101L, 3010102L),
                           UTDANN = c(1L, 1L, 3L, 2L, 3L), VAL2 = c(1L, 1L, 1L, 1L, 1L),
                           grunnkrets = c(3010101L, 3010101L, 3010101L, 3010101L, 3010102L),
                           kommune = c(NA, 301L, 301L, 301L, 301L),
                           fylke = c(3L, 3L, 3L, 3L, 3L),
                           bydel = c(30116L, 30116L, 30116L, 30116L, NA)),
                      row.names = c(NA, -5L), class = c("data.table", "data.frame"))

  dtoutb <- dt[1:4]

  ## TEST ------------
  expect_equal(is_level_na(dt = data.table::copy(dt), "fylke"), dtoutf)
  expect_equal(is_level_na(dt = data.table::copy(dt), "kommune"), dtoutk)
  expect_equal(is_level_na(dt = data.table::copy(dt), "bydel"), dtoutb)
})
