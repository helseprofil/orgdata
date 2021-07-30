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
  message(file)
  stop(sprintf("Unable to read file `%s` format", ext))
}

#' @method find_data csv
#' @export
find_data.csv <- function(file, ...) {
  data.table::fread(input = file, ...)
}

#' @method find_data xls
#' @export
find_data.xls <- function(file, ...) {
  readxl::read_excel(path = file, ...)
}

#' @method find_data xlsx
#' @export
find_data.xlsx <- find_data.xls
