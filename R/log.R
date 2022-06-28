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
#' # To list codes with xxxx9999 of koblid 67
#' read_log("code99", 67)
#' }
#' @export
read_log <- function(name = NULL, koblid = NULL){
  # name - Name of object log
  is_null(name, "Log filename is missing!")
  ## is_null(koblid, "KOBLID is missing")

  if (is.null(koblid)){
    logFile <- paste0(name, ".csv")
  } else {
    logFile <- paste0(name, "_koblid", koblid, ".csv")
  }

  orgPath <- is_orgdata_path()
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
is_log_write <- function(value = NULL, x = NULL, koblid = NULL, format = "vector"){
  # value - Object to put in log
  # x  - name the object
  # format - Object time as "vector", "dt" etc
  orgpath <- is_orgdata_path()

  if (is.null(koblid)){
    nameFile <- paste0(x, ".csv")
    logFun <- paste0('`', 'read_log("', x, '")`')
  } else {
    nameFile <- paste0(x, "_koblid", koblid, ".csv")
    logFun <- paste0('`', 'read_log("', x, '", koblid)`')
  }

  fileName <- file.path(orgpath, nameFile)
  outCmd <- tryCatch({
    if (format == "dt"){
      data.table::fwrite(x = value, file = fileName)
    } else {
      data.table::fwrite(x = list(V1 = value), file = fileName)
    }
    logFun
  },
  error = function(err){
    assign(x = x, value = value, envir = log)
    paste0("log$", x)
  })

  invisible(outCmd)
}
