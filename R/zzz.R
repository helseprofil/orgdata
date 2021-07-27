## Additional options specific to orgdata
## use getOption("orgdata.folder") to get default folder
## or options("orgdata.file" = "dbfile.accdb") to change the filename

## Options for Folkehelseprofiler
op.orgdata <- list(
  orgdata.folder = "Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/STYRING",
  orgdata.file = "org-innlesing.accdb",
  orgdata.rawdata = "Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/ORGDATA",
  orgdata.geofile = ""
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
