#' @keywords internal
#' @title Process Raw Data
#' @description Implement the specifications to the raw data and deliver the output
#' @param file File rawdata
#' @param filespec Specification for a file from `tbl_Innlesing`
#' @param fgspec Specification for a file group
#' @param con Connection to the database
#' @param verbose Make processes explicit. Default is FALSE
#' @param row Select specific row only
#' @param control Logical value. If the file has been checked for possible errors
#' @param .log For logbook
#' @return A dataset with `data.table` format

is_org_process <- function(file,
                           filespec,
                           fgspec,
                           con,
                           verbose = getOption("orgdata.verbose"),
                           row = getOption("orgdata.debug.row"),
                           control = FALSE,
                           .log = parent.frame()
                           ) {
  GEO <- NULL

  dots <- get_innlesarg(spec = filespec)

  ## For GEO codes that are derived from a combination of two columns
  geoVals <- is_separate(filespec$GEO, ",")
  geo2col <- length(geoVals) > 1
  if (geo2col) {
    dots <- is_geo_split(geo = geoVals, dots = dots)
  }

  ## With or without dots or extra arguments
  extra <- get_extra_args(spec = filespec)
  if (is.na(dots[1])) {
    dt <- is_read_file(file = file, extra = extra)
  } else {
    dt <- is_read_file_dots(file = file, dots = dots, extra = extra)
  }

  ## From options(orgdata.debug.row)
  if (!is.null(row)){
    dt <- dt[row,]
  }

  ## GEO codes from two columns needs to be joined
  if (geo2col){
    dt[, GEO := paste0(get(geoVals[1]), get(geoVals[2]))]
    dt[, (geoVals) := NULL]
  }

  ## Logging
  ## .log$logr$rawcat <- unique(dt[[4]])

  manSpec <- get_manheader(spec = filespec)
  dt <- do_manheader(dt, manSpec)

  colSpec <- get_column_standard(spec = filespec)
  dt <- do_column_standard(dt, colSpec)

  dt <- do_delete_row(dt = dt, spec = filespec, con = con)

  splitSpec <- get_split(spec = fgspec)
  dt <- do_split(dt = dt, split = splitSpec)

  yrSpec <- get_year(filespec, con)
  dt <- do_year(dt, yrSpec)

  dt <- do_mutate(dt, spec = filespec)

  reshSpec <- get_reshape_id_val(dt, spec = filespec)
  dt <- do_reshape(dt, reshSpec)

}

## Helper -------------------------------------

## GEO of a combined two or more columns
is_geo_split <- function(geo, dots){
  ## fread style args use colClasses
  colStr <- rep("character", 2)
  colStr <- stats::setNames(colStr, geo)

  if (is.na(dots)){
    dots <- list(colClasses = colStr)
  } else {
    dots$colClasses = colStr
  }

  return(dots)
}

is_read_file <- function(file, debug = getOption("orgdata.debug.nrow"), extra){
  if (debug > 0) {
    dt <- read_file(file = file, nrows = debug)
  } else {
    dt <- read_file(file = file)
  }

  dt <- do_extra_args(dt = dt, args = extra)
}

is_read_file_dots <- function(file, dots, debug = getOption("orgdata.debug.nrow"), extra){
  if (debug > 0){
    dots$nrows <- debug
    dt <- read_file(file = file, dots)
  } else {
    dt <- read_file(file = file, dots)
  }

  dt <- do_extra_args(dt = dt, args = extra)
}
