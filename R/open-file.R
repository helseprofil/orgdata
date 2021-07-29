#' Open Rawdata for Reading
#' @description
#' Open rawdata and load it into memory for further process.
#' But you have to set the class attribute of the file prior to
#' using [open_file()] function. Otherwise, use [read_file()] function
#' instead.
#' @param file Input file
#' @param ... Other options relevant to the file type
#' @examples
#' \dontrun{
#' file <- "path/to/my-data.csv"
#' class(file) <- "csv"
#' dt <- open_file(file)
#' }
#' @export
open_file <- function(file = NULL, ...) {
  UseMethod("open_file", file)
}

#' @method open_file default
#' @export
open_file.default <- function(file, ...) {
  fileExt <- tools::file_ext(file)
  message(sprintf("Can't process file `%s` format", fileExt))
  return(file)
}

#' @method open_file csv
#' @export
open_file.csv <- function(file, ...) {
  args <- list(...)
  dt <- data.table::fread(input = file)
  return(dt)
}

#' @method open_file xls
#' @export
open_file.xls <- function(file, ...) {
  args <- list(...)
}
