## Path for different operating system
OS <- Sys.info()["sysname"]

## Options for Folkehelseprofiler
optOrg <- yaml::yaml.load_file("https://raw.githubusercontent.com/helseprofil/config/main/config-orgdata.yml")
opt.orgdata <- as.list(opt_rename(optOrg))

.onLoad <- function(libname, pkgname) {
  op <- options()
  orgDT <- !(names(opt.orgdata) %in% names(op))
  if (any(orgDT)) options(opt.orgdata[orgDT])
  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("orgdata version 0.6.2")
}
