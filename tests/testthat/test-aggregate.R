test_that("Aggregate file", {

  aggDT <- readRDS(system.file("testdata", "aggregate-dt.rds", package = "orgdata"))
  geoDT <- readRDS(system.file("testdata", "geo-aggregate-dt.rds", package = "orgdata"))
  outDT <- readRDS(system.file("testdata", "aggregate-out.rds", package = "orgdata"))
  outChkDT <- readRDS(system.file("testdata", "aggregate-check-dt.rds", package = "orgdata"))

  expect_equal(do_aggregate(dt = aggDT,
                            source = "grunnkrets",
                            level = "kommune",
                            year = 2021,
                            geoDT = geoDT,
                            wide = as.character(1:3)), outDT)

  expect_equal(do_aggregate(dt = aggDT,
                            source = "grunnkrets",
                            level = "kommune",
                            year = 2021,
                            geoDT = geoDT,
                            check = TRUE,
                            wide = as.character(1:3)), outChkDT)
})

test_that("Aggregate group list", {

  ## DATA ----------------
  srcCols <- c("kommune", "AAR", "ALDER", "UTDANN", "TAB1", "LANDBAK", "INNVKAT")

  output <- list(c("kommune", "AAR", "ALDER", "TAB1", "UTDANN"),
                 c("kommune", "AAR", "ALDER", "TAB1", "LANDBAK"),
                 c("kommune", "AAR", "ALDER", "TAB1", "INNVKAT"),
                 c("kommune", "AAR", "ALDER", "TAB1", "UTDANN", "LANDBAK"),
                 c("kommune", "AAR", "ALDER", "TAB1", "UTDANN", "INNVKAT"),
                 c("kommune", "AAR", "ALDER", "TAB1", "LANDBAK", "INNVKAT"),
                 c("kommune", "AAR", "ALDER", "TAB1", "UTDANN", "LANDBAK", "INNVKAT"),
                 c("kommune", "AAR", "ALDER", "TAB1"))

  ## TEST --------------
  expect_equal(is_set_list(level = "kommune", srcCols = srcCols, dt = DTagg), output)


})


test_that("Aggregate group list with AGGKOL", {


  ## DATA ------------
  level <- "bydel"
  srcCols <- c("bydel", "AAR", "ALDER", "UTDANN", "LANDBAK", "INNVKAT")
  colx <- "ALDER"

  output <- list(c("bydel", "AAR", "ALDER"),
                 c("bydel", "AAR", "UTDANN"),
                 c("bydel", "AAR", "LANDBAK"),
                 c("bydel", "AAR", "INNVKAT"),
                 c("bydel", "AAR", "ALDER", "UTDANN"),
                 c("bydel", "AAR", "ALDER", "LANDBAK"),
                 c("bydel", "AAR", "ALDER", "INNVKAT"),
                 c("bydel", "AAR", "UTDANN", "LANDBAK"),
                 c("bydel", "AAR", "UTDANN", "INNVKAT"),
                 c("bydel", "AAR", "LANDBAK", "INNVKAT"),
                 c("bydel", "AAR", "ALDER", "UTDANN", "LANDBAK"),
                 c("bydel", "AAR", "ALDER", "UTDANN", "INNVKAT"),
                 c("bydel", "AAR", "ALDER", "LANDBAK", "INNVKAT"),
                 c("bydel", "AAR", "UTDANN", "LANDBAK", "INNVKAT"),
                 c("bydel", "AAR", "ALDER", "UTDANN", "LANDBAK", "INNVKAT"),
                 c("bydel", "AAR"))

  ##TEST -------------
  expect_equal(is_set_list(level = level, srcCols = srcCols, colx = colx, dt = DTagg), output)
})


test_that("Get aggregate levels", {

  #DATA -----------------------
  spec <-structure(list(ID = 1L, FILGRUPPE = "TEST01", UTMAPPE = "SSB\\BEFOLKNING\\ORG\\2023",
                        AGGREGERE = "B,F", AGGKOL = "KJONN", KOLNAVN = "VAL1=ANTALL",
                        SPLITTFRA = "LANDSSB", SPLITTTIL = "LANDBAK, INNVKAT"),
                   class = "data.frame", row.names = c(NA, -1L))

  output <- c(B = "bydel", F = "fylke")

  ## TEST -------------
  reset_opt()
  expect_equal(get_aggregate(spec = spec), output)

  options(orgdata.debug.geo = TRUE)
  expect_equal(get_aggregate(spec = spec), "kommune")
  reset_options()

  options(orgdata.debug.aggregate = TRUE)
  expect_equal(get_aggregate(spec = spec), "kommune")
  reset_options()

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


test_that("Column with NA", {

  ## DATA
  n = 24L
  set.seed(25)
  DTNA <- data.table::data.table(
    color = sample(c("green","yellow","red", NA), n, TRUE),
    year = as.Date(sample(paste0(2011:2015,"-01-01"), n, TRUE)),
    status = as.factor(sample(c("removed","active","inactive","archived", NA), n, TRUE)),
    amount = sample(1:5, n, TRUE),
    value = sample(c(3, 3.5, 2.5, 2), n, TRUE)
  )


  ## TEST
  expect_error(is_validate_NA(cols = c("color", "status"), dt = DTNA))

})
