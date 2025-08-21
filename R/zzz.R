## Path for different operating system
OS <- Sys.info()["sysname"]

## Global options
opt.orgdata <- is_globs("orgdata")

.onLoad <- function(libname, pkgname) {
  op <- options()
  optOrg <- is_globs("orgdata")
  orgDT <- !(names(optOrg) %in% names(op))
  if (any(orgDT)) options(optOrg[orgDT])
  
  corrglobs <- is_correct_globs(optOrg)
  if(!isTRUE(corrglobs)){
    x <- utils::askYesNo("Options are not the same as in the config file, update options now?")
    if(isTRUE(x)){
      update_globs("orgdata")
    }
  }
  
  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(orgdata_logo())
  packageStartupMessage(paste("           Version",
                              utils::packageDescription("orgdata")[["Version"]], "\n"))

  latest <- is_latest_version()
  if (latest){
    x <- utils::askYesNo("Update orgdata now?")
    if (isTRUE(x)){
      orgdata::update_khpackage("orgdata")
    }
  }
}

rvers <- function(){
  rlokal <- paste(version[c("major", "minor")], collapse = ".")
  numeric_version(rlokal) > numeric_version("4.1.0")
}
