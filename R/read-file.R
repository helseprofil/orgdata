#' Read Data File
#' @description Read rawdata. It uses the [find_data()] generic method.
#'    For a \code{ .csv } file, [data.table::fread()] is used and all the arguments
#'    for \code{fread} function can be used. For a \code{ xlsx } or \code{ .xls } file
#'    [readxl::read_excel()] function is used and all its arguments.
#' @inheritParams find_data
#' @examples
#' \dontrun{
#' rdata <- read_file("/file/path/mydata.xlsx", sheet = "S3", range = cell_rows(1:4))
#' rdata <- read_file("/file/path/mydata.csv", sep = ",", header = FALSE)
#' }
#' @export
read_file <- function(file = NULL, ...) {
  is_null(file)
  ext <- tools::file_ext(file)
  class(file) <- append(class(file), ext)
  find_data(file, ...)
}
