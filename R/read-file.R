#' Read Data File
#' @description Read the rawdata
#' @inheritParams find_data
#' @export
read_file <- function(file = NULL, ...) {
  check_null(file)
  file <- identify_file(file)
  find_data(file, ...)
}


identify_file <- function(x) {
  fileExt <- tools::file_ext(x)

  cls <- switch(fileExt,
    csv = "csv",
    xlsx = "xls",
    xls = "xls"
  )

  class(x) <- append(class(x), cls)
  return(x)
}
