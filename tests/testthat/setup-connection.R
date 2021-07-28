
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
