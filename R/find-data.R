#' Find Rawdata
#' @description
#' Find rawdata and load it into memory for further process.
#' It requires the class attribute of the file is set prior to
#' using [find_data()] method. Otherwise, use [read_file()] function
#' instead unless you have specific reason not to.
#' @param file Input file
#' @param ... Other options relevant to the file type
#' @examples
#' \dontrun{
#' file <- "path/to/my-data.csv"
#' class(file) <- "csv"
#' dt <- find_data(file)
#' }
#' @export
find_data <- function(file = NULL, ...) {
  UseMethod("find_data", file)
}

#' @method find_data default
#' @export
find_data.default <- function(file, ...) {
  ext <- tools::file_ext(file)
  message("File: ", file)
  stop(sprintf("File not found! Unable to read file `%s` format", ext))
}

#' @method find_data csv
#' @export
find_data.csv <- function(file, ...) {
  dots <- is_csv_dots(...)
  is_verbose(file, msg = "File:")
  dots$file <- file
  dt <- do.call(data.table::fread, dots)
}

#' @method find_data txt
#' @export
find_data.txt <- find_data.csv

#' @method find_data fhi
#' @export
find_data.fhi <- find_data.csv

#' @method find_data none
#' @export
find_data.none <- find_data.csv

#' @method find_data http
#' @export
find_data.http <- function(file, ...){
  dots <- is_csv_dots(...)
  is_verbose(file, msg = "File:")
  dots$input <- file
  dt <- do.call(data.table::fread, dots)
}


#' @method find_data xls
#' @export
find_data.xls <- function(file, ...) {
  headerRename <- is.element("header", names(list(...)))

  dots <- is_xls_dots(...)
  is_verbose(file, msg = "File:")
  dots$path <- file
  dt <- do.call(readxl::read_xls, dots)

  if (headerRename){
    dt <- is_header_name(dt)
  }
  return(dt)
}

#' @method find_data xlsx
#' @export
find_data.xlsx <- function(file, ...) {
  headerRename <- is.element("header", names(list(...)))

  dots <- is_xls_dots(...)
  is_verbose(file, msg = "File:")
  dots$path <- file
  dt <- do.call(readxl::read_xlsx, dots)

  if (headerRename){
    dt <- is_header_name(dt)
  }
  return(dt)
}

#' @method find_data dta
#' @export
find_data.dta <- function(file, ...){
  dots <- is_dta_dots(...)
  is_verbose(file, msg = "File:")
  dots$file <- file
  do.call(haven::read_dta, dots)
}

#' @method find_data sav
#' @export
find_data.sav <- function(file, ...){
  dots <- is_sav_dots(...)
  is_verbose(file, msg = "File:")
  dots$file <- file
  do.call(haven::read_sav, dots)
}

## Helper -------------------------------------------
is_csv_dots <- function(...){
  if(length(list(...)) > 0){
    dots <- is_args(...)
    dots <- is_dt_args(dots)
  } else {
    dots <- list()
  }
  return(dots)
}

is_xls_dots <- function(...){
  if (length(list(...)) > 0){
    dots <- is_args(...)
    dots <- is_xls_args(dots)
  } else {
    dots <- list()
  }
  return(dots)
}

is_dta_dots <- function(...){
  if (length(list(...)) > 0){
    dots <- is_args(...)
    dots <- is_dta_args(dots)
  } else {
    dots <- list()
  }
  return(dots)
}

is_sav_dots <- function(...){
  if (length(list(...)) > 0){
    dots <- is_args(...)
    dots <- is_sav_args(dots)
  } else {
    dots <- list()
  }
  return(dots)
}

## Direct args or those from registration database
is_args <- function(...){
  dd <- list(...)[[1]]
  if (is.list(dd)){
    dots <- dd
  } else {
    dots <- list(...)
  }
  return(dots)
}

## Arguments in fread that have numeric input
is_dt_args <- function(x){
  x <- is_rename_args(from = "trimws", to = "strip.white")
  x <- is_rename_args(from = "na", to = "na.strings")
  argInt <- c("skip", "nrows", "drop")
  inx <- is.element(argInt, names(x))
  elm <- argInt[inx]

  if (sum(inx)>0){
    x <- is_numeric_args(x, elm)
  }
  return(x)
}

## Arguments in read_excel that have numeric input
is_xls_args <- function(x){
  x <- is_rename_args(from = "nrows", to = "n_max")
  x <- is_rename_args(from = "trimws", to = "trim_ws")
  x <- is_rename_args(from = "header", to = "col_names")
  argInt <- c("skip", "n_max")
  inx <- is.element(argInt, names(x))
  elm <- argInt[inx]

  if (sum(inx)>0){
    x <- is_numeric_args(x, elm)
  }
  return(x)
}

is_sav_args <- function(x){
  x <- is_rename_args(from = "nrows", to = "n_max")
  x <- is_rename_args(from = "na", to = "user_na")
  argInt <- c("skip", "n_max")
  inx <- is.element(argInt, names(x))
  elm <- argInt[inx]

  if (sum(inx)>0){
    x <- is_numeric_args(x, elm)
  }
  return(x)
}

is_dta_args <- function(x){
  x <- is_rename_args(from = "nrows", to = "n_max")
  argInt <- c("skip", "n_max")
  inx <- is.element(argInt, names(x))
  elm <- argInt[inx]

  if (sum(inx)>0){
    x <- is_numeric_args(x, elm)
  }
  return(x)
}

## Use standard args in orgdata and rename it to the respective
## read functions arguments
is_rename_args <- function(from, to, .env = parent.frame()){
  x <- .env$x
  idx <- which(names(x) == from)
  names(x)[idx] <- to
  return(x)
}

is_numeric_args <- function(x = NULL, elm = NULL){
  for (i in seq_along(elm)){
    val <- as.numeric(x[elm[i]][1])
    x[elm[i]] <- val
  }
  return(x)
}

## Standard readxl header style ie. ...1, ...2 etc needs
## to be renamed to V1,V2 etc similar fread style
is_header_name <- function(x){
  vars <- paste0("V", 1:ncol(x))
  data.table::setnames(x, old = names(x), new = vars)
}
