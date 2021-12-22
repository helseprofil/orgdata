#' Environment to store log info
#' @export log
log <- new.env()


#' @title Read Log File
#' @description Read the log file from orgdata default path. To check the
#'   default path use `orgdata:::is_orgdata_path()`.
#' @param name Log filename without file extention
#' @param koblid Connecting ID or `KOBLID`
#' @examples
#' \dontrun{
#' read_log("code99")
#' }
#' @export
read_log <- function(name = NULL, koblid = NULL){
  # name - Name of object log
  is_null(name, "Log filename is missing!")
  is_null(koblid, "KOBLID is missing")

  orgPath <- is_orgdata_path()
  logFile <- paste0(name, "_koblid", koblid, ".csv")
  logPath <- file.path(orgPath, logFile)
  fileExist <- fs::file_exists(logPath)

  if (fileExist){
    is_verbose(x = logPath, msg = "File: ")
    data.table::fread(file = logPath)
  } else {
    is_stop("File not found!", var = logPath)
  }
}

## Helper --------------
is_log <- function(value = NULL, x = NULL){
  # value - Object to put in log
  # x - name the object
  assign(x = x, value = value, envir = log)
}

is_log_write <- function(value = NULL, x = NULL, koblid = NULL){
  # value - Object to put in log
  # x  - name the object
  orgpath <- is_orgdata_path()
  nameFile <- paste0(x, "_koblid", koblid, ".csv")

  outCmd <- tryCatch({
    data.table::fwrite(x = list(value), file = file.path(orgpath, nameFile))
    paste0('`', 'read_log("', x, '", koblid)`')
  },
  error = function(err){
    is_log(value = value, x = x)
    paste0("log$", x)
  })

  invisible(outCmd)
}
