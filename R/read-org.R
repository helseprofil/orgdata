#' Read and Run the Specifications
#' @description
#' Read the specifications in the register database and implement them
#' to the selected group of files (\emph{filgruppe}). All files under
#' the selected group will be considered unless the \code{KOBLID} with
#' argument \code{koblid} is specified. Specifying \code{koblid} is useful for testing.
#'
#' The function [lesorg()] is an
#' alias to [read_org()].
#' @examples
#' \dontrun{
#' read_org("BEFOLKNING")
#' read_org("BEFOLKNING", koblid = 19)
#' read_org("BEFOLKNING", koblid = c(15, 50))
#' }
#' @param group The group of files (\emph{filgruppe})
#' @param koblid \code{KOBLID} from table \emph{tbl_Koble}
#' @aliases read_org lesorg
#' @import data.table
#' @export
read_org <- function(group = NULL, koblid = NULL) {
  is_null(group, "Filgruppe is missing")
  dbFile <- is_path_db(check = TRUE)

  # CONN ------------------------------------------
  kh <- KHelse$new(dbFile)

  # SPECS -----------------------------------------
  spec <- find_spec(
    file = "specification.sql",
    value = group,
    con = kh$dbconn
  )
  ## data.table::setDT(spec)
  ## TODO Can't use DT yet. Some functions are still
  ## based on DF eg. find_column_input

  fgSpec <- find_spec(
    file = "filegroups.sql",
    value = group,
    con = kh$dbconn
  )
  ## data.table::setDT(fgSpec)

  # SELECT FILE ------------------------------------------
  spec <- is_org_files(spec = spec, id = koblid)
  rowFile <- nrow(spec)
  message(group, " has ", rowFile, " valid file(s).")

  # PROCESS ---------------------------------------------
  verbose <- getOption("orgdata.verbose")
  DT <- vector(mode = "list", length = rowFile)
  for (i in seq_len(rowFile)) {
    fileSpec <- spec[i, ]
    filePath <- is_path_raw(fileSpec, check = TRUE)

    if (verbose){
      koblid <- fileSpec$KOBLID
      fileN <- fileSpec$FILNAVN
      message("Koblid: ", koblid, " File: ", fileN)
    }

    dt <- is_org_process(
      file = filePath,
      filespec = fileSpec,
      fgspec = fgSpec,
      con = kh$dbconn,
      verbose = verbose
    )

    DT[[i]] <- dt
    gc()
  }

  on.exit(kh$db_close(), add = TRUE)
  out <- data.table::rbindlist(DT, fill = TRUE)

}


#' @export
#' @rdname read_org
lesorg <- read_org

## Helper functions ---------------------------------------------------------
## Create complete path to DB file
is_path_db <- function(check = FALSE) {
  db <- file.path(
    getOption("orgdata.drive"),
    getOption("orgdata.folder.db"),
    getOption("orgdata.db")
  )

  if (isTRUE(check) && isFALSE(file.exists(db))) {
    stop("Access database file does not exist! \n", db)
  }

  return(db)
}

## Create complete path to raw data file
is_path_raw <- function(spec, check = FALSE) {
  filename <- find_column_input(spec, "FILNAVN")
  filePath <- file.path(getOption("orgdata.folder.raw"), filename)

  if (isTRUE(check) && isFALSE(file.exists(filePath))) {
    stop("File does not exist! \n", filePath)
  }

  return(filePath)
}

# Exclude files after KOBLID and IBRUKTIL
is_org_files <- function(spec, id = NULL) {
  IBRUKTIL <- NULL
  koblid <- spec$KOBLID
  ## TODO Implement spec as DT from parent.env
  data.table::setDT(spec)
  if (!is.null(id)) {
    koblid <- id
    spec <- spec[spec$KOBLID %in% koblid, ]
  }

  spec[, IBRUKTIL := as.Date(IBRUKTIL, format = "%Y-%m-%d")]
  spec <- spec[IBRUKTIL == as.Date("9999-01-01", format = "%Y-%m-%d"), ]

  nfile <- nrow(spec)
  if (nfile == 0) {
    stop("No valid file to be processed!")
  }
  data.table::setDF(spec)
}
