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
  if(length(list(...)) > 0){
    dots <- is_args(...)
    dots <- is_dt_var(dots)
  } else {
    dots <- list()
  }

  is_verbose(file, msg = "File:")
  dots$file <- file
  do.call(data.table::fread, dots)
}

#' @method find_data fhi
#' @export
find_data.fhi <- find_data.csv


#' @method find_data xls
#' @export
find_data.xls <- function(file, ...) {
  if (length(list(...)) > 0){
    dots <- is_args(...)
    dots <- is_xls_var(dots)
  } else {
    dots <- list()
  }

  is_verbose(file, msg = "File:")
  dots$path <- file
  do.call(readxl::read_excel, dots)
}

#' @method find_data xlsx
#' @export
find_data.xlsx <- find_data.xls

## Helper -------------------------------------------
## Direct args or from registration database
is_args <- function(...){
  ## take it out from nested list
  dd <- list(...)[[1]]
  if (is.list(dd)){
    dots <- dd
  } else {
    dots <- list(...)
  }
  return(dots)
}

## For arguments in fread that have numeric input
is_dt_var <- function(x){
  argInt <- c("skip", "nrows", "drop")
  inx <- is.element(argInt, names(x))
  elm <- argInt[inx]

  if (sum(inx)>0){
    x <- is_numeric_var(x, elm)
  }
  return(x)
}

## For arguments in read_excel that have numeric input
is_xls_var <- function(x){
  argInt <- c("skip", "n_max")
  inx <- is.element(argInt, names(x))
  elm <- argInt[inx]

  if (sum(inx)>0){
    x <- is_numeric_var(x, elm)
  }
  return(x)
}

is_numeric_var <- function(x = NULL, elm = NULL){
  for (i in seq_along(elm)){
    val <- as.numeric(x[elm[i]][1])
    x[elm[i]] <- val
  }
  return(x)
}
