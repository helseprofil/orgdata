devtools::load_all()

devtools::install()
library(orgdata)

op <- options()
options(orgdata.folder.db = "Helseprofiler/DB_helseprofil")
options(orgdata.db = "org-innlesing.accdb")
getOption("orgdata.drive") <- "N:"
getOption("orgdata.folder.db")
regFile <- "org-innlesing.accdb"
regDB <- file.path(getOption("orgdata.drive"), getOption("orgdata.folder.db"), regFile)

regDB
kh <- KHelse$new(regDB)
kh$dbconn
kh$dbname
# kh$db_close()
# kh$db_connect()

dbfile <- "N:/Helseprofiler/DB_helseprofil/org-innlesing.accdb"

## LesOrg
options(op)
getOption("orgdata.folder.db")
getOption("orgdata.db")
dd <- read_org("Dode")
lesorg("TEST01")

## Get Specification
file <- "specification.sql"
query <- find_query(file, "Dode")
query
spec <- find_spec(file, "Dode", kh$dbconn)
spec

# External SQL
dd <- find_spec("c:/Users/ybka/Git-fhi/orgdata/inst/specification.sql", "Dode", kh$dbconn, external = TRUE)
dd

## TODO - get_filgruppe(filgruppe)

## File year
(qr <- find_query("file-year.sql", 15))
(sp <- find_spec("file-year.sql", 15, kh$dbconn))
(yr <- is_defaar(15, kh$dbconn))
class(yr)

get_year(spec[1, ], kh$dbconn)
spec[1, ]

## Innlesarg
## TODO - Error if EXTRA column uses "," to sperarate arguments
(input <- get_innlesarg("ark", spec$INNLESARG))
