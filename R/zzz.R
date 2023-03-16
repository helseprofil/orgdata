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
  print.orgdata_logo(orgdata_logo())

  packageStartupMessage(paste("          Version",
                              utils::packageDescription("orgdata")[["Version"]], "\n"))

  latest <- is_latest_version()
  if (latest){
    x <- utils::askYesNo("Update orgdata now?")
    if (isTRUE(x)){
      orgdata::update_orgdata()
    }
  }
}
