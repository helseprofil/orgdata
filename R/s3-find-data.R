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
    dots <- is_dt_args(dots)
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
    dots <- is_xls_args(dots)
  } else {
    dots <- list()
  }

  is_verbose(file, msg = "File:")
  dots$path <- file
  do.call(readxl::read_xls, dots)
}

#' @method find_data xls
#' @export
find_data.xlsx <- function(file, ...) {
  if (length(list(...)) > 0){
    dots <- is_args(...)
    dots <- is_xls_args(dots)
  } else {
    dots <- list()
  }

  is_verbose(file, msg = "File:")
  dots$path <- file
  do.call(readxl::read_xlsx, dots)
}

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
is_dt_args <- function(x){
  x <- is_rename_args(from = "trimws", to = "strip.white")
  argInt <- c("skip", "nrows", "drop")
  inx <- is.element(argInt, names(x))
  elm <- argInt[inx]

  if (sum(inx)>0){
    x <- is_numeric_args(x, elm)
  }
  return(x)
}

## For arguments in read_excel that have numeric input
is_xls_args <- function(x){
  x <- is_rename_args(from = "nrows", to = "n_max")
  x <- is_rename_args(from = "trimws", to = "trim_ws")
  argInt <- c("skip", "n_max")
  inx <- is.element(argInt, names(x))
  elm <- argInt[inx]

  if (sum(inx)>0){
    x <- is_numeric_args(x, elm)
  }
  return(x)
}

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
