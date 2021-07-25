devtools::load_all()

devtools::install()
library(orgdata)

op <- options()
options("orgdata.folder" = "Helseprofiler/DB_helseprofil")
osDrive <- "N:"
getOption("orgdata.folder")
regFile <- "org-innlesing.accdb"
regDB <- file.path(osDrive, getOption("orgdata.folder"), regFile)

regDB
kh <- KHelse$new(regDB)
kh$dbconn
kh$dbname
kh$db_close()
kh$db_connect()


## Get Specification
file <- "specification.sql"
query <- get_query(file, "Dode")
query
spec <- read_spec(file, "TEST01", kh$dbconn)
spec

# External SQL
dd <- read_spec("c:/Users/ybka/Git-fhi/orgdata/inst/specification.sql", "Dode", kh$dbconn, external = TRUE)
dd

## TODO - get_filgruppe(filgruppe)

## Innlesarg
## TODO - Error if EXTRA column uses "," to sperarate arguments
(input <- get_innlesarg("ark", spec$INNLESARG))
