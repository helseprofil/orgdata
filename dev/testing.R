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
spec <- get_spec(file, "Dode", kh$dbconn)
spec

## TODO - get_filgruppe(filgruppe)
