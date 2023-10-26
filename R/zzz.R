## Path for different operating system
OS <- Sys.info()["sysname"]

## Global options
opt.orgdata <- is_globs()

.onLoad <- function(libname, pkgname) {
  op <- options()
  optOrg <- is_globs()
  orgDT <- !(names(optOrg) %in% names(op))
  if (any(orgDT)) options(optOrg[orgDT])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  print(orgdata_logo())
  packageStartupMessage(paste("           Version",
                              utils::packageDescription("orgdata")[["Version"]], "\n"))

  latest <- is_latest_version()
  if (latest){
    x <- utils::askYesNo("Update orgdata now?")
    if (isTRUE(x)){
      orgdata::update_orgdata()
    }
  }
}

rvers <- function(){
  rlokal <- paste(version[c("major", "minor")], collapse = ".")
  numeric_version(rlokal) > numeric_version("4.1.0")
}

