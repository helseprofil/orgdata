#' Read Data File
#' @description Read rawdata either using `filid` value or complete file path. It uses the [find_data()] generic method.
#'    For a \code{ .csv } file, [data.table::fread()] is used and all other arguments
#'    for \code{fread} function can be used. For a \code{ xlsx } or \code{ .xls } file
#'    [readxl::read_excel()] function and all of its arguments.
#' @inheritParams find_data
#' @param ... All other arguments to be passed related to the file format
#' @examples
#' \dontrun{
#' # With FILID
#' DT <- read_file(file = 5)
#'
#' # With filepath
#' rdata <- read_file(file = "/file/path/mydata.xlsx", sheet = "S3", range = cell_rows(1:4))
#' rdata <- read_file(file = "/file/path/mydata.csv", sep = ",", header = FALSE)
#' }
#' @export
read_file <- function(file = NULL, ...) {
  is_null(file)

  if (is.numeric(file)){
    file <- is_id_file(filid = file)
  }

  ext <- tools::file_ext(file)
  class(file) <- append(class(file), ext)

  find_data(file, ...)
}


## Helper -------------------------------------

is_id_file <- function(filid = NULL, con = NULL){
  if (is.null(con)){
    dbFile <- is_path_db(db = getOption("orgdata.db"),
                         check = TRUE)
    kh <- KHelse$new(dbFile)
    con <- kh$dbconn
  }

  file <- find_spec("org-file.sql", value = filid, con = con)
  fileName <- file.path(getOption("orgdata.folder.raw"), file)
}
