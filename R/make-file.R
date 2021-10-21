#' Implement the Specifications
#' @description
#' Make a `csv` file with the specifications in the register database and implement them
#' to the raw data of the selected group of files (\emph{filgruppe}). All files under
#' the selected group will be affected unless the \code{KOBLID} with
#' argument \code{koblid} is specified. Specifying \code{koblid} is useful
#' especially for testing purposes.
#' @description
#' The function [lag_fil()] is an alias to [make_file()].
#' @examples
#' \dontrun{
#' make_file("BEFOLKNING")
#' make_file("BEFOLKNING", koblid = 19)
#' make_file("BEFOLKNING", koblid = c(15, 50))
#' }
#' @param group The group of files (\emph{filgruppe})
#' @param koblid \code{KOBLID} from table \emph{tbl_Koble}
#' @param aggregate Aggregate data according to the specification in registration database.
#'    Default is `TRUE`. Global options with `orgdata.aggregate`.
#' @param save Save as `.csv` by activating `save_file()`. Default is `FALSE`
#' @inheritParams do_aggregate
#' @param implicitnull Default is `TRUE` to add implicit null to the dataset. Global options
#'   with `orgdata.implicit.null`.
#' @aliases make_file lag_fil
#' @importFrom data.table `:=` `%chin%`
#' @importFrom crayon `%+%`
#' @export
make_file <- function(group = NULL,
                      koblid = NULL,
                      aggregate = getOption("orgdata.aggregate"),
                      save = FALSE,
                      year = NULL,
                      implicitnull = getOption("orgdata.implicit.null")
                      ) {
  is_null(group, "Filgruppe is missing")
  is_debug()

  dbFile <- is_path_db(
    db = getOption("orgdata.db"),
    check = TRUE
  )

  ## CONNECTION --------------------------------------------
  kh <- is_conn_db(dbFile)
  on.exit(kh$db_close(), add = TRUE)

  ## SPECIFICATIONS ----------------------------------------
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
  grpMsg <- paste0("File(s) to be processed in ", group, ":")
  is_colour_txt(x = rowFile, grpMsg, type = "note")

  ## COLUMNS TO KEEP ---------------------------------------
  dataCols <- is_data_cols(fgspec = fgSpec)

  ## PROCESS ON FILES LEVEL IN A FILGRUPPE -----------------------
  DT <- vector(mode = "list", length = rowFile)
  for (i in seq_len(rowFile)) {
    fileSpec <- spec[i, ]
    filePath <- is_path_raw(fileSpec, check = TRUE)

    is_verbose(msg = "----------------------------------", type = "other")
    is_verbose(fileSpec$KOBLID, "Koblid:")

    dt <- is_org_process(
      file = filePath,
      filespec = fileSpec,
      fgspec = fgSpec,
      con = kh$dbconn
    )

    ## Keep columname as TAB1 to 3 and VAL1 to 3 as defined in Access coz
    ## aggregating uses the standard columnames for id and measure variables
    dt <- do_reshape_rename_col(dt = dt, spec = fileSpec)
    dt <- do_recode(dt = dt, spec = fileSpec, con = kh$dbconn)

    ## Only columns defined in tbl_Filgruppe will be kept
    deleteVar <- setdiff(names(dt), dataCols)
    if (length(deleteVar) != 0) {
      dt[, (deleteVar) := NULL]
    }

    if (length(deleteVar) != 0) {
      msgWarn <- "Some columns aren't defined in FILGRUPPE. They are now deleted!"
      is_verbose(x = msgWarn, type = "warn")
      deleteVar <- paste(deleteVar, collapse = ", ")
      is_verbose(x = paste_cols(deleteVar), "Deleted column(s):", type = "warn2")
    }

    ## TODO - Not sure if this necessary
    ## convert some columns to interger. Must be after
    ## the variables are recoded eg. INNKAT is string before recorded to number
    ## dt <- is_col_int(dt)

    if (implicitnull){
      dnull <- do_implicit_null(dt)
      if (nrow(dnull) > 0){
        is_verbose(x = nrow(dnull), msg = "Number of row(s) for implicit null:")
        dt <- data.table::rbindlist(list(dt, dnull))
        data.table::setkeyv(dt, "AAR")
      }
    }

    if (aggregate) {
      dt <- is_aggregate(dt, fgspec = fgSpec, year = year)
    }

    DT[[i]] <- dt
    gc()
  }

  ## PROCESS ON FILGRUPPE LEVEL ----------------------------------
  grpCols <- get_colname(spec = fgSpec)
  outDT <- do_colname(
    data.table::rbindlist(DT, fill = TRUE),
    cols = grpCols)

  outDT <- do_recode_aggregate(dt = outDT, spec = fileSpec, con = kh$dbconn)

  standardCols <- is_standard_cols()
  orderCols <- intersect(standardCols, names(outDT))
  data.table::setcolorder(outDT, orderCols)

  if (save) {
    save_file(dt = outDT, group = group, fgSpec = fgSpec)
  }

  return(outDT[])
}

#' @export
#' @rdname make_file
lag_fil <- make_file


#' @title Implement the Specifications
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `read_raw` was renamed to `make_file()` to make it more clear
#' what the function is doing.
#' @keywords internal
#' @export
read_raw <- function(group = NULL,
                     koblid = NULL,
                     aggregate = getOption("orgdata.aggregate"),
                     save = FALSE,
                     year = NULL) {

  lifecycle::deprecate_stop("0.0.9", "read_raw()", "make_file()")
  ## make_file(group, koblid, aggregate, save, year)
}

#' @export
#' @rdname read_raw
lesraw <- function(group = NULL,
                   koblid = NULL,
                   aggregate = getOption("orgdata.aggregate"),
                   save = FALSE,
                   year = NULL) {

  lifecycle::deprecate_stop("0.0.9", "lesraw()", "lag_fil()")
  ## make_file(group, koblid, aggregate, save, year)
}



## Helper -----------------------------------------
## Helper functions are in file utils-read-org.R
