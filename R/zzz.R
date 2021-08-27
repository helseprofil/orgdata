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
  orgdata.folder.db = "Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/STYRING/raw-khelse",
  orgdata.folder.raw = "F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/ORGDATA",
  orgdata.folder.output = "F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/CSV",
  orgdata.db = "raw-khelse_BE.accdb",
  orgdata.geo = "geo-koder.accdb",
  orgdata.verbose = FALSE,
  orgdata.columns = c("GEO", "AAR", "KJONN", "ALDER", "UTDANN", "LANDBAK", "VAL"),
  orgdata.int = c("GEO", "AAR", "KJONN", "ALDER", "VAL"),
  orgdata.aggregate = TRUE,
  orgdata.bugs = FALSE,
  orgdata.active = TRUE
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  orgDT <- !(names(opt.orgdata) %in% names(op))
  if (any(orgDT)) options(opt.orgdata[orgDT])
  invisible()
}
