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
  orgdata.folder.data = "F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/ORGDATA",
  orgdata.db = "raw-khelse_BE.accdb",
  orgdata.geo = "geo-koder.accdb",
  orgdata.verbose = FALSE,
  orgdata.aggregate = TRUE,
  orgdata.debug = FALSE,

  ## Standard columns
  orgdata.columns = c("GEO", "AAR", "KJONN", "ALDER", "UTDANN", "LANDSSB",
                      "TAB1", "TAB2", "TAB3", "VAL1", "VAL2", "VAL3"),

  ## Columns with integer values. GEO is not included because some geo codes has
  ## to be a combination of 2 columns and need to keep the leading 0 on second
  ## column before merging. GEO will only be converted to integer after merging
  orgdata.int = c("AAR", "KJONN", "ALDER", "VAL1"),

  ## Either to change columnames to standard names or keep as it's.
  ## Default is to change to standard
  orgdata.active = TRUE
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  orgDT <- !(names(opt.orgdata) %in% names(op))
  if (any(orgDT)) options(opt.orgdata[orgDT])
  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("orgdata version 0.0.8 - alpha")
}
