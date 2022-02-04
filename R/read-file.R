#' Read Data File
#' @description Read rawdata either using `FILID` value or complete file path. It uses the [find_data()] generic method.
#'    For a \code{ .csv } file, [data.table::fread()] is used and all other arguments
#'    for \code{fread} function can be used. For a \code{ .xlsx } or \code{ .xls } file
#'    [readxl::read_excel()] function and all of its arguments.
#' @description Nevertheless, some most used arguments are standardized for `read_file()` and there are:
#' \itemize{
#'   \item `nrows` to display maksimum numbers to read
#'   \item `header` FALSE to give default columnames as `V1`, `V2` etc
#'   \item `skip` a specific number of raws before reading the data
#'   \item `trimws` to trim leading and trailing whitespace
#'   \item `na` for character value to be interpreted as `NA`
#' }
#' @param file Use FILID, FILEGROUP or a complete path of a filename
#' @param ... All other arguments to be passed related to the file format
#' @examples
#' \dontrun{
#' # With FILID
#' DT <- read_file(file = 5)
#' DT <- read_file(file = 5, fill = TRUE, nrows = 10)
#'
#' # With FILEGROUP
#' dt <- read_file(file = "UTFORE")
#'
#' # With filepath
#' rdata <- read_file(file = "/file/path/mydata.xlsx", sheet = "S3", range = cell_rows(1:4))
#' rdata <- read_file(file = "/file/path/mydata.csv", sep = ",", header = FALSE)
#' }
#' @importFrom methods is
#' @export
read_file <- function(file = NULL, ...) {
  is_debug()
  is_null(file)

  if (is.numeric(file)) {
    file <- is_read_id(filid = file)
  } else {
    file <- is_read_path(file = file)
  }

  web <- is_read_http(file = file, check = TRUE)

  ## Data on the web or direct file
  if (web){
    file <- is_read_http(file)
  } else {
    fileExist <- fs::file_exists(file)
    if (isFALSE(fileExist)){
      is_stop("File not found!", file)
    }

    ext <- tools::file_ext(file)
    if (ext == ""){
      class(file) <- append(class(file), "none")
    } else {
      class(file) <- append(class(file), ext)
    }
  }


  dt <- find_data(file, ...)

  if (isFALSE(is(dt, "data.table"))){
    data.table::setDT(dt)
  }

  return(dt)
}

#' @export
#' @rdname read_file
les_fil <- read_file


## Helper -------------------------------------

is_read_id <- function(filid = NULL, con = NULL) {
  is_debug(deep = TRUE)
  if (is.null(con)) {
    dbFile <- is_path_db(
      db = getOption("orgdata.db"),
      check = TRUE
    )
    kh <- is_conn_db(dbFile)
    con <- kh$dbconn
  }

  on.exit(kh$db_close(), add = TRUE)
  file <- find_spec("org-file.sql", value = filid, con = con)
  file.path(getOption("orgdata.folder.data"), file)
}

is_read_path <- function(file){
  slash01 <- grepl("\\\\", file)
  slash02 <- grepl("/", file)
  path <- slash01 + slash02

  # get path of FILGRUPPE if not direct file path
  if (path == 0){
    file <- is_file_csv(group = file, action = "read")
  }
  return(file)
}

is_read_http <- function(file, check = FALSE){
  web <- FALSE
  http <- grepl("^http", file)
  if (isTRUE(http)){
    class(file) <- append(class(file), "http")
    web <- TRUE
  }

  if (isTRUE(check)){
    return(web)
  } else {
    return(file)
  }
}
