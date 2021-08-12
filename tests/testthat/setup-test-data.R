
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
  INNLESARG = "header=TRUE, skip=TRUE, ark=Sheet1", MANHEADER = "2,3=KJONN,ALDER",
  GEO = NA_character_, AAR = "aar", KJONN = NA_character_, ALDER = NA_character_,
  UTDANN = NA_character_, LANDBAK = NA_character_, VAL = NA_character_,
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
  LANDBAK = "ethnic", VAL = "sum", EXTRA = NA_character_
), class = "data.frame", row.names = c(
  NA,
  -1L
))

## BEFOLKNING spec
befolk <- structure(list(
  ID = 11L, FILGRUPPE = "BEFOLKNING", AGGREGERE = "F,K",
  ADDKOL = NA_character_, ADDVAL = NA_character_, SPLITTFRA = "LANDBAK",
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
), LANDBAK = c(
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
##   ADDKOL = NA_character_, ADDVAL = NA_character_, SPLITTFRA = "LANDBAK",
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
), LANDBAK = c(
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


### Data for test recode ----------------------------------

recDT <- structure(list(AAR = c(2019L, 2019L, 2019L),
                        GEO = c(3010105L, 3010105L, 3010202L),
                        KJONN = c(1L, 1L, 1L),
                        ALDER = c(67L, 71L, 69L),
                        UTDANN = 1:3, LANDBAK = c("1B", "0", "0"),
                        SIVILSTAND = c(2L, 4L, 1L),
                        VAL = c(1L, 1L, 1L), LANDB = c("1", "0", "0"),
                        LANDF = c("2", "1", "1")),
                   row.names = c(NA, -3L), class = c("data.table", "data.frame"
                                                     ))
                                        # Codebook
recCB <- structure(list(FILGRUPPE = c("Dode", "Dode"),
                        LESID = c(NA, 16L), KOL = c("LANDF", "LANDB"),
                        TYPE = c(1L, 1L), FRA = c("NA", "0"),
                        TIL = c("1", "9")),
                   class = c("data.table", "data.frame"), row.names = c(NA, -2L))

recOut <- structure(list(AAR = c(2019L, 2019L, 2019L),
                         GEO = c(3010105L, 3010105L, 3010202L),
                         KJONN = c(1L, 1L, 1L), ALDER = c(67L, 71L, 69L),
                         UTDANN = 1:3, LANDBAK = c("1B", "0", "0"),
                         SIVILSTAND = c(2L, 4L, 1L),
                         VAL = c(1L, 1L, 1L), LANDB = c("1", "9", "9"),
                         LANDF = c("2", "1", "1")),
                    row.names = c(NA, -3L), class = c("data.table", "data.frame"))
