## Additional options specific to orgdata
## use getOption("orgdata.folder") to get default folder
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.orgdata <- list(
    orgdata.folder = "Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/ORGDATA",
    orgdata.register = "Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/STYRING",
    orgdata.geo = ""
  )
  toset <- !(names(op.orgdata) %in% names(op))
  if (any(toset)) options(op.orgdata[toset])

  invisible()
}

## Path for different operating system
OS <- Sys.info()["sysname"]
osDrive <- switch(OS,
                  Linux = "/mnt/F",
                  Windows = "F:"
                  )
