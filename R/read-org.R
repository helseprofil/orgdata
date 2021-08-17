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
#' @param save Save as `.csv` by activating `save_file()`. Default is `FALSE`
#' @param ... Additional parameters
#' @aliases read_org lesorg
#' @import data.table
#' @export
read_org <- function(group = NULL,
                     koblid = NULL,
                     aggregate = getOption("orgdata.aggregate"),
                     save =FALSE,
                     year = NULL,
                     ...) {
  is_null(group, "Filgruppe is missing")
  dbFile <- is_path_db(db = getOption("orgdata.db"),
                       check = TRUE)

  # CONNECTION--------------------------------------
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

  ## SELECT FILES ------------------------------------------
  spec <- is_org_files(spec = spec, id = koblid)
  rowFile <- nrow(spec)
  message(group, " has ", rowFile, " valid file(s).")

  ## COLUMNS TO KEEP -------------------------------------
  dataCols <- is_data_cols(fgspec = fgSpec)

  ## PROCESS ---------------------------------------------

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

    ## Only columns defined in tbl_Filgruppe will be kept
    deleteVar <- setdiff(names(dt), dataCols)
    if (length(deleteVar)!=0) {
      dt[, (deleteVar) := NULL]
    }

    if (length(deleteVar)!= 0 && isTRUE(getOption("orgdata.verbose"))){
      deleteVar <- paste(deleteVar, collapse = ", ")
      message("Column(s) are deleted from the dataset: ", deleteVar)
    }

    ## convert some columns to interger. Must be after
    ## the variables are recoded eg. LANDF is string before recorded to number
    dt <- is_col_int(dt)

    if (aggregate){
      dt <- is_aggregate(dt, fgspec = fgSpec, year = year, ...)
    }

    DT[[i]] <- dt
    gc()
  }

  on.exit(kh$db_close(), add = TRUE)
  out <- data.table::rbindlist(DT, fill = TRUE)

  if (save) save_file(dt = out, group = group)

  return(out)
}


#' @export
#' @rdname read_org
lesorg <- read_org


## Helper functions are in file utils-read-org.R
