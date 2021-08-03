## Additional options specific to orgdata
## use getOption("orgdata.folder") to get default folder
## or options("orgdata.db" = "dbfile.accdb") to change the filename

## Options for Folkehelseprofiler
op.orgdata <- list(
  orgdata.folder = "Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/STYRING",
  orgdata.db = "org-innlesing.accdb",
  orgdata.rawdata = "Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/ORGDATA",
  orgdata.geofile = "",
  orgdata.columns = c("GEO", "AAR", "KJONN", "ALDER", "UTDANN", "LANDBAK", "VAL")
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  orgDT <- !(names(op.orgdata) %in% names(op))
  if (any(orgDT)) options(op.orgdata[orgDT])
  invisible()
}

## Path for different operating system
OS <- Sys.info()["sysname"]
osDrive <- switch(OS,
  Linux = "/mnt/F",
  Windows = "F:"
)
