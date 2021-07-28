devtools::load_all()

devtools::install()
library(orgdata)

op <- options()
options("orgdata.folder" = "Helseprofiler/DB_helseprofil")
options("orgdata.file" = "org-innlesing.accdb")
osDrive <- "N:"
getOption("orgdata.folder")

regDB <- file.path(osDrive, getOption("orgdata.folder"), regFile)

regDB
kh <- KHelse$new(regDB)
kh$dbconn
kh$dbname
# kh$db_close()
# kh$db_connect()

dbfile <- "N:/Helseprofiler/DB_helseprofil/org-innlesing.accdb"

## LesOrg
getOption("orgdata.folder")
getOption("orgdata.file")
dd <- read_org("Dode")
lesorg("TEST01")

## Get Specification
file <- "specification.sql"
query <- find_query(file, "Dode")
query
spec <- find_spec(file, "TEST01", kh$dbconn)
spec

# External SQL
dd <- find_spec("c:/Users/ybka/Git-fhi/orgdata/inst/specification.sql", "Dode", kh$dbconn, external = TRUE)
dd

## TODO - get_filgruppe(filgruppe)

## File year
(qr <- find_query("file-year.sql", 15))
(sp <- find_spec("file-year.sql", 15, kh$dbconn))
(yr <- find_year(15, kh$dbconn))
class(yr)

## Innlesarg
## TODO - Error if EXTRA column uses "," to sperarate arguments
(input <- get_innlesarg("ark", spec$INNLESARG))
