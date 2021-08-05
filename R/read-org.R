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
#' @import data.table
#' @export
read_org <- function(group = NULL, id = NULL) {
  is_null(group, "Filgruppe is missing")
  dbFile <- is_db_file(check = TRUE)

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

  fgspec <- find_spec(
    file = "filegroups.sql",
    value = group,
    con = kh$dbconn
  )
  ## data.table::setDT(fgspec)

  # SELECT FILE ------------------------------------------
  spec <- is_org_files(spec = spec, id = id)
  rowFile <- nrow(spec)
  message(group, " has ", rowFile, " valid file(s).")

  # PROCESS ---------------------------------------------
  DT <- vector(mode = "list", length = rowFile)
  for (i in seq_len(rowFile)) {
    filespec <- spec[i, ]
    filepath <- is_raw_file(filespec, check = TRUE)

    dt <- read_file(file = filepath)

    colSpec <- get_column_standard(spec = filespec)
    dt <- do_column_standard(dt, colSpec)
    ## TODO Any extra args for file specific from INNLESARG

    splitSpec <- get_split(spec = fgspec)
    dt <- do_split(dt = dt, split = splitSpec)

    yrSpec <- get_year(filespec, kh$dbconn)
    dt <- do_year(dt, yrSpec)

    manSpec <- get_manheader(spec = filespec)
    dt <- do_manheader(dt, manSpec)

    DT[[i]] <- dt
  }

  out <- data.table::rbindlist(DT, fill = TRUE)
}


#' @export
#' @rdname read_org
lesorg <- read_org

## Helper functions ---------------------------------------------------------
## Create complete path to DB file
is_db_file <- function(check = FALSE) {
  db <- file.path(
    getOption("orgdata.drive"),
    getOption("orgdata.folder"),
    getOption("orgdata.db")
  )

  if (isTRUE(check) && isFALSE(file.exists(db))) {
    stop("Access database file does not exist! \n", db)
  }

  return(db)
}

## Create complete path to raw data file
is_raw_file <- function(spec, check = FALSE) {
  filename <- find_column_input(spec, "FILNAVN")
  filepath <- file.path(getOption("orgdata.drive"), getOption("orgdata.rawdata"), filename)

  if (isTRUE(check) && isFALSE(file.exists(filepath))) {
    stop("File does not exist! \n", filepath)
  }

  return(filepath)
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
