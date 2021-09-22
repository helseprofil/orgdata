#' Read and Implement the Specifications
#' @description
#' Read the specifications in the register database and implement them
#' to the raw data of the selected group of files (\emph{filgruppe}). All files under
#' the selected group will be affected unless the \code{KOBLID} with
#' argument \code{koblid} is specified. Specifying \code{koblid} is useful
#' especially for testing purposes.
#'
#' The function [lesraw()] is an
#' alias to [read_raw()].
#' @examples
#' \dontrun{
#' read_raw("BEFOLKNING")
#' read_raw("BEFOLKNING", koblid = 19)
#' read_raw("BEFOLKNING", koblid = c(15, 50))
#' }
#' @param group The group of files (\emph{filgruppe})
#' @param koblid \code{KOBLID} from table \emph{tbl_Koble}
#' @param aggregate Aggregate data according to the specification in registration database.
#'    Default is FALSE. Use `options(orgdata.aggregate = TRUE)` to change globally.
#' @inheritParams do_aggregate
#' @param save Save as `.csv` by activating `save_file()`. Default is `FALSE`
#' @inheritParams do_aggregate
#' @param ... Additional parameters
#' @aliases read_raw lesraw
#' @importFrom data.table `:=` `%chin%`
#' @export
read_raw <- function(group = NULL,
                     koblid = NULL,
                     aggregate = getOption("orgdata.aggregate"),
                     save = FALSE,
                     year = NULL,
                     geo = NULL,
                     val = NULL,
                     ...) {
  is_null(group, "Filgruppe is missing")
  is_debug()

  dbFile <- is_path_db(
    db = getOption("orgdata.db"),
    check = TRUE
  )

  ## CONNECTION--------------------------------------
  kh <- is_conn_db(dbFile)

  ## SPECS -----------------------------------------
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
  message(group, " has ", rowFile, " file(s) to be processed...")

  ## COLUMNS TO KEEP ---------------------------------------
  dataCols <- is_data_cols(fgspec = fgSpec)

  ## PROCESS ON FILES IN A FILGRUPPE -----------------------
  DT <- vector(mode = "list", length = rowFile)
  for (i in seq_len(rowFile)) {
    fileSpec <- spec[i, ]
    filePath <- is_path_raw(fileSpec, check = TRUE)

    is_verbose(fileSpec$KOBLID, "Koblid:")

    dt <- is_org_process(
      file = filePath,
      filespec = fileSpec,
      fgspec = fgSpec,
      con = kh$dbconn
    )

    dt <- do_recode(dt = dt, spec = fileSpec, con = kh$dbconn)

    ## Only columns defined in tbl_Filgruppe will be kept
    deleteVar <- setdiff(names(dt), dataCols)
    if (length(deleteVar) != 0) {
      dt[, (deleteVar) := NULL]
    }

    if (length(deleteVar) != 0) {
      msgWarn <- "Some columns aren't defined in FILGRUPPE. They are now deleted"
      is_verbose(x = msgWarn, type = "warning")
      deleteVar <- paste(deleteVar, collapse = ", ")
      is_verbose(deleteVar, "Deleted column(s):", type = "message")
    }

    ## convert some columns to interger. Must be after
    ## the variables are recoded eg. LANDF is string before recorded to number
    dt <- is_col_int(dt)

    if (aggregate) {
      dt <- is_aggregate(dt,
                         fgspec = fgSpec,
                         year = year,
                         geo = geo,
                         val = val,
                         ...)
    }

    DT[[i]] <- dt
    gc()
  }

  on.exit(kh$db_close(), add = TRUE)

  ## PROCESS ON FILGRUPPE ----------------------------------
  grpCols <- get_colname(spec = fgSpec)
  outDT <- do_colname(
    data.table::rbindlist(DT, fill = TRUE),
    cols = grpCols)

  ## REORDER COLS --------------------------------------------
  orderCols <- intersect(getOption("orgdata.columns"), names(outDT))
  data.table::setcolorder(outDT, orderCols)

  if (save) save_file(dt = outDT, group = group)

  return(outDT)
}


#' @export
#' @rdname read_raw
lesraw <- read_raw


## Helper functions are in file utils-read-org.R
