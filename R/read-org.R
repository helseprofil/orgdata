#' Read and Run the Specifications
#' @description
#' Read the specifications in the register database and implement them
#' to the selected group of files (\emph{filgruppe}). All files under
#' the selected group will be considered unless the \code{KOBLID} with
#' argument \code{id} is specified. Specifying \code{id} is useful for testing.
#'
#' The function [lesorg()] is an
#' alias to [read_org()].
#' @examples
#' \dontrun{
#' read_org("BEFOLKNING")
#' read_org("BEFOLKNING", id = 19)
#' read_org("BEFOLKNING", id = c(15, 50))
#' }
#' @param group The group of files (\emph{filgruppe})
#' @param id \code{KOBLID} from table \emph{tbl_Koble}
#' @aliases read_org lesorg
#' @export
read_org <- function(group = NULL, id = NULL) {
  is_null(group, "Filgruppe is missing")
  dbFile <- is_db_file()
  kh <- KHelse$new(dbFile)

  spec <- find_spec(
    file = "specification.sql",
    value = group,
    con = kh$dbconn
  )

  koblid <- spec$KOBLID
  if (!is.null(id)) {
    koblid <- id
    spec <- spec[spec$KOBLID %in% koblid, ]
  }

  message(group, " has ", length(koblid), " file(s).")

  DT <- vector(mode = "list", length = length(koblid))

  for (i in seq_len(length(koblid))) {
    filespec <- spec[i, ]
    filepath <- is_raw_file(filespec)

    ## TODO Any extra args for file specific from INNLESARG
    dt <- do_rename_col_standard(filepath, filespec)
    DT[[i]] <- dt
  }

  out <- data.table::rbindlist(DT)
}


#' @export
#' @rdname read_org
lesorg <- read_org

## Helper functions ---------------------------------------------------------
## Create complete path to DB file
is_db_file <- function() {
  db <- file.path(
    getOption("orgdata.drive"),
    getOption("orgdata.folder"),
    getOption("orgdata.db")
  )

  if (isFALSE(file.exists(db))) {
    stop("Access database file does not exist! \n", db)
  }

  return(db)
}

## Create complete path to raw data file
is_raw_file <- function(spec) {
  filename <- find_column_input(spec, "FILNAVN")
  filepath <- file.path(getOption("orgdata.drive"), getOption("orgdata.rawdata"), filename)

  if (isFALSE(file.exists(filepath))) {
    stop("File does not exist! \n", filepath)
  }

  return(filepath)
}
