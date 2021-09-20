#' @keywords internal
#' @title Process Raw Data
#' @description Implement the specifications to the raw data and deliver the output
#' @param file File rawdata
#' @param filespec Specification for a file from `tbl_Innlesing`
#' @param fgspec Specification for a file group
#' @param con Connection to the database
#' @param verbose Make processes explicit. Default is FALSE
#' @return A dataset with `data.table` format

is_org_process <- function(file,
                           filespec,
                           fgspec,
                           con,
                           verbose = getOption("orgdata.verbose"),
                           .log = parent.frame()
                           ) {
  dots <- get_innlesarg(spec = filespec)

  ## For GEO codes that are derived from a combination of two columns
  geoVals <- is_separate(filespec$GEO, ",")
  geo2col <- length(geoVals) > 1
  if (geo2col) {
    dots <- is_geo_split(geo = geoVals, dots = dots)
  }

  if (is.na(dots[1])) {
    dt <- read_file(file = file)
  } else {
    dt <- read_file(file = file, dots)
  }

  ## GEO codes from two columns needs to be joined
  if (geo2col){
    dt[, GEO := paste0(get(geoVals[1]), get( geoVals[2] ))]
    dt[, (geoVals) := NULL]
  }

  ## Logging
  .log$logr$rawcat <- unique(dt[[4]])

  colSpec <- get_column_standard(spec = filespec)
  dt <- do_column_standard(dt, colSpec)
  ## TODO Any extra args for file specific from INNLESARG

  splitSpec <- get_split(spec = fgspec)
  dt <- do_split(dt = dt, split = splitSpec)

  yrSpec <- get_year(filespec, con)
  dt <- do_year(dt, yrSpec)

  manSpec <- get_manheader(spec = filespec)
  dt <- do_manheader(dt, manSpec)

  ## dataCols <- get_addcols(spec = fgspec)
  ## dt <- do_addcols(dt, cols = dataCols)
}

## Helper -------------------------------------

## GEO of a combined two or more columns
is_geo_split <- function(geo, dots){
  ## fread style args
  colStr <- rep("character", 2)
  colStr <- setNames(colStr, geo)
  dots$colClasses = colStr
  return(dots)
}
