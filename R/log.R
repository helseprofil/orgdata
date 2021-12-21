#' Environment to store log info
#' @export log
log <- new.env()


#' @title Read Log File
#' @description Read the log file from orgdata default path. To check the
#'   default path use `orgdata:::is_orgdata_path()`.
#' @param name Log filename without file extention
#' @examples
#' \dontrun{
#' read_log("code99")
#' }
#' @export
read_log <- function(name = NULL){
  # x - Name of object log
  is_null(name, "Log filename is missing!")
  orgPath <- is_orgdata_path()
  logFile <- paste0(name, ".csv")
  logPath <- file.path(orgPath, logFile)
  is_verbose(x = logPath, msg = "File: ")
  data.table::fread(file = logPath)
}

## Helper --------------
is_log <- function(value = NULL, x = NULL){
  # value - Object to put in log
  # x - name the object
  assign(x = x, value = value, envir = log)
}

is_log_write <- function(value = NULL, x = NULL){
  # value - Object to put in log
  # x  - name the object

  orgpath <- is_orgdata_path()
  nameFile <- paste0(x, ".csv")
  data.table::fwrite(x = list(value), file = file.path(orgpath, nameFile))
}

