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
#' @param aggregate Aggregate data according to the specification in registration database.
#'    Default is FALSE. Use `options(orgdata.aggregate = TRUE)` to change globally.
#' @inheritParams do_aggregate
#' @param ... Additional parameters
#' @aliases read_org lesorg
#' @import data.table
#' @export
read_org <- function(group = NULL,
                     koblid = NULL,
                     aggregate = getOption("orgdata.aggregate"),
                     year = NULL,
                     ...) {
  is_null(group, "Filgruppe is missing")
  dbFile <- is_path_db(db = getOption("orgdata.db"),
                       check = TRUE)

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

  DT <- vector(mode = "list", length = rowFile)
  for (i in seq_len(rowFile)) {
    fileSpec <- spec[i, ]
    filePath <- is_path_raw(fileSpec, check = TRUE)

    if (getOption("orgdata.verbose")){
      koblid <- fileSpec$KOBLID
      fileN <- fileSpec$FILNAVN
      message("Koblid: ", koblid, " File: ", fileN)
    }

    dt <- is_org_process(
      file = filePath,
      filespec = fileSpec,
      fgspec = fgSpec,
      con = kh$dbconn
    )

    dt <- do_recode(dt = dt, spec = fileSpec, con = kh$dbconn)

    if (aggregate){
      dt <- is_aggregate(dt, fgspec = fgSpec, year = year, ...)
    }

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

is_aggregate <- function(dt, fgspec, verbose = getOption("orgdata.verbose"), year = year, ...){
  if(verbose){
    message("Aggregating data ...")
  }

  aggSpec <- get_aggregate(spec = fgspec)
  source <- is_geo_level(dt$GEO[1])

  nSpec <- length(aggSpec)
  DT <- vector(mode = "list", length = nSpec)
  for (i in seq_len(nSpec)){
    dtt <- data.table::copy(dt)
    dtt <- do_aggregate(dt=dtt, source = source, level = aggSpec[i], year = year, ...)
    DT[[i]] <- dtt
    gc()
  }
  rm(dtt)
  data.table::rbindlist(DT, use.names = TRUE, fill = TRUE)
}

## identify geo level
is_geo_level <- function(x){
  geo <- nchar(x)
  data.table::fcase(geo %in% 7:8, "g",
                    geo %in% 5:6, "b",
                    geo %in% 3:4, "k",
                    geo %in% 1:2, "f")

}


## Create complete path to DB file
is_path_db <- function(db, check = FALSE) {
                                        # db - Database file
  db <- file.path(
    getOption("orgdata.drive"),
    getOption("orgdata.folder.db"),
    db
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
