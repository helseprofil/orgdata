## Additional options specific to orgdata
## use getOption("orgdata.folder.db") to get default folder
## or options(orgdata.db = "dbfile.accdb") to change the filename


## Path for different operating system
OS <- Sys.info()["sysname"]
sysDrive <- switch(OS,
  Linux = "/mnt/F",
  Windows = "F:"
)

## Options for Folkehelseprofiler
opt.orgdata <- list(
  orgdata.drive = sysDrive,
  orgdata.folder.db = "Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/STYRING",
  orgdata.folder.raw = "Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/ORGDATA",
  orgdata.folder.output = "",
  orgdata.db = "org-innlesing.accdb",
  orgdata.geo = "",
  orgdata.verbose = FALSE,
  orgdata.columns = c("GEO", "AAR", "KJONN", "ALDER", "UTDANN", "LANDBAK", "VAL")
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  orgDT <- !(names(opt.orgdata) %in% names(op))
  if (any(orgDT)) options(opt.orgdata[orgDT])
  invisible()
}
