
## This is only applicable to test locally and not via CI/CD ----------
## Uncomment the code below for local testing
## --------------------------------------------------------------------
## dbfile <- "N:/Helseprofiler/DB_helseprofil/org-innlesing.accdb"
## kh <- KHelse$new(dbfile)

## Dataset 01 ---------------------------------------------------------
df01 <- structure(list(
  KOBLID = 20L, FILID = 15L, FILGRUPPE = "TEST01",
  FILNAVN = "Path/to/testfile.csv",
  IBRUKTIL = structure(253370764800, class = c(
    "POSIXct",
    "POSIXt"
  ), tzone = "UTC"),
  LESID = 18L, FILGRUPPE = "TEST01",
  INNLESARG = "header=TRUE | skip=TRUE | ark=Sheet1", MANHEADER = "2=KJONN,3=ALDER",
  GEO = NA_character_, AAR = "aar", KJONN = NA_character_, ALDER = NA_character_,
  UTDANN = NA_character_, LANDSSB = NA_character_, VAL = NA_character_,
  EXTRA = "sep=,"
), class = "data.frame", row.names = c(NA, -1L))


## Has error for MANHEADER
df02 <- structure(list(
  KOBLID = 21L, FILID = 16L, FILGRUPPE = "TEST02",
  FILNAVN = "testpath/test02.xlsx", IBRUKTIL = structure(253370764800, class = c(
    "POSIXct",
    "POSIXt"
  ), tzone = "UTC"), LESID = 19L, FILGRUPPE = "TEST02",
  INNLESARG = "1 = AGE", MANHEADER = "3,5=AGE", GEO = "geo",
  AAR = "year", KJONN = "gender", ALDER = "age", UTDANN = "edu",
  LANDSSB = "ethnic", VAL = "sum", EXTRA = NA_character_
), class = "data.frame", row.names = c(
  NA,
  -1L
))

## BEFOLKNING spec
befolk <- structure(list(
  ID = 11L, FILGRUPPE = "BEFOLKNING", AGGREGERE = "F,K",
  KOLNAVN = NA_character_, ADDVAL = NA_character_, SPLITTFRA = "LANDSSB",
  SPLITTTIL = "landb, landf"
), class = "data.frame", row.names = c(
  NA,
  -1L
))

## Dataset --------------------------------
DT01 <- structure(list(GEO = c(
  "0806", "210", "0109", "0305", "903",
  "0405", "0606", "0804", "0203", "1404"
), KJONN = c(
  1L, 2L, 2L,
  2L, 2L, 2L, 2L, 2L, 1L, 1L
), ALDER = c(
  74L, 23L, 31L, 28L, 8L,
  9L, 34L, 20L, 12L, 37L
), LANDSSB = c(
  "0", "0", "0", "0", "1C",
  "2C", "3B", "0", "0", "2B"
), VAL = c(
  2L, 23L, 2L, 10L, 2L, 1L,
  1L, 1L, 1L, 1L
)), row.names = c(NA, -10L), class = c(
  "data.table",
  "data.frame"
))


## fspec <- structure(list(
##   ID = 11L, FILGRUPPE = "BEFOLKNING", AGGREGERE = "F,K",
##   KOLNAVN = NA_character_, ADDVAL = NA_character_, SPLITTFRA = "LANDSSB",
##   SPLITTTIL = "landb, landf"
## ), class = "data.frame", row.names = c(
##   NA,
##   -1L
## ))

dfna <- data.frame(INNLESARG = NA_character_, MANHEADER = NA_character_)

DFout <- structure(list(GEO = c(
  "0806", "210", "0109", "0305", "903",
  "0405", "0606", "0804", "0203", "1404"
), KJONN = c(
  1L, 2L, 2L,
  2L, 2L, 2L, 2L, 2L, 1L, 1L
), ALDER = c(
  74L, 23L, 31L, 28L, 8L,
  9L, 34L, 20L, 12L, 37L
), LANDSSB = c(
  "0", "0", "0", "0", "1C",
  "2C", "3B", "0", "0", "2B"
), VAL = c(
  2L, 23L, 2L, 10L, 2L, 1L,
  1L, 1L, 1L, 1L
), landb = c(
  "0", "0", "0", "0", "1", "2", "3",
  "0", "0", "2"
), landf = c(
  NA, NA, NA, NA, "C", "C", "B", NA,
  NA, "B"
)), row.names = c(NA, -10L), class = "data.frame")


## SPLITT DATA -----------------------------------------------
dataSplit <- structure(list(GEO = c(3010102L, 3010102L, 3010102L, 3010102L, 3010102L),
                            KJONN = c(1L, 1L, 1L, 1L, 1L), ALDER = c(0L, 1L, 4L, 6L, 18L),
                            LANDSSB = c("1C", "0", "1C", "3B", "0"), VAL1 = c(1L, 1L, 1L, 1L, 1L)),
                       row.names = c(NA, -5L), class = c("data.frame"))
data.table::setDT(dataSplit)

spt <- list(from = "LANDSSB", to = c("LANDB", "INNVKAT"))
