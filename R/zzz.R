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
  orgdata.verbose = TRUE,
  orgdata.aggregate = TRUE,
  orgdata.implicit.null = TRUE,

  ## Standard columns
  orgdata.columns = c("GEO", "LEVEL", "AAR", "KJONN", "ALDER", "UTDANN", "LANDSSB",
                      "TAB1", "TAB2", "TAB3", "VAL1", "VAL2", "VAL3"),

  ## Columns with integer values. GEO is not included because some geo codes has
  ## to be a combination of 2 columns and need to keep the leading 0 on second
  ## column before merging. GEO will only be converted to integer after merging
  orgdata.int = c("AAR", "KJONN", "ALDER", "VAL1"),

  ## Use for debuging purposes only
  orgdata.debug = FALSE,
  orgdata.debug.nrow = FALSE  #First 20 rows only if TRUE
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  orgDT <- !(names(opt.orgdata) %in% names(op))
  if (any(orgDT)) options(opt.orgdata[orgDT])
  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("orgdata version 0.1.0")
}
