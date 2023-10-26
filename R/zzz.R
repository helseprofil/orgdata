## Path for different operating system
OS <- Sys.info()["sysname"]

## Global options
is_options <- function(){
  optOrg <- yaml::yaml.load_file("https://raw.githubusercontent.com/helseprofil/config/main/config-orgdata.yml")
  as.list(opt_rename(optOrg))
}
opt.orgdata <- is_options()

.onLoad <- function(libname, pkgname) {
  op <- options()
  optOrg <- is_options()
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

