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

## Get Specification
spec <- get_spec("Dode", kh$dbconn)
spec
