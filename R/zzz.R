## Path for different operating system
OS <- Sys.info()["sysname"]
sysDrive <- switch(OS,
  Linux = "/mnt/F",
  Windows = "F:"
)

## Options for Folkehelseprofiler
opt.orgdata <- list(
  orgdata.year = NULL, # which year of geo codes for recoding should refer to
  orgdata.drive = sysDrive,
  orgdata.folder.db = "Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/STYRING/raw-khelse",
  orgdata.folder.data = "F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/ORGDATA",
  orgdata.db = "raw-database_BE.accdb",
  orgdata.geo = "geo-koder.accdb",
  orgdata.verbose = TRUE,
  orgdata.aggregate = TRUE,
  orgdata.implicit.null = TRUE,
  orgdata.recode.base = FALSE,
  orgdata.parallel = FALSE,

  ## Number of TABS and VALS
  orgdata.tabs = 3,
  orgdata.vals = 3,

  ## Standard columns
  orgdata.columns = c("GEO", "LEVEL", "AAR", "KJONN", "ALDER", "UTDANN", "LANDSSB"),

  ## Columns with integer values. GEO is not included because some geo codes has
  ## to be a combination of 2 columns and need to keep the leading 0 on second
  ## column before merging. GEO will only be converted to integer after merging
  orgdata.integer = c("AAR", "KJONN", "ALDER"),

  ## Use for debuging purposes only
  orgdata.debug = FALSE,
  orgdata.debug.nrow = 0,  #If other than 0 then x rows will be selected
  orgdata.debug.row = NULL, #Select specific rows eg. 20:50
  orgdata.debug.aggregate = FALSE, #Recode but don't aggregate if TRUE
  orgdata.debug.geo = FALSE, #Keep old geo if TRUE

  ## Just for fun
  orgdata.emoji = "mark" #can choose mark, thumb, smile or sad
)


.onLoad <- function(libname, pkgname) {
  op <- options()
  orgDT <- !(names(opt.orgdata) %in% names(op))
  if (any(orgDT)) options(opt.orgdata[orgDT])
  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("orgdata version 0.5.3")
}
