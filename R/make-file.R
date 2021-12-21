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
#' @param aggregate Logical argument. Default is `TRUE`. Aggregate data according
#'    to the specification in registration database.
#'    Global options with `orgdata.aggregate`.
#' @param save Save as `.csv` by activating `save_file()`. Default is `FALSE`
#' @inheritParams do_aggregate
#' @param implicitnull Logical argument. Default is `TRUE` to add implicit
#'   null to the dataset. Global options with `orgdata.implicit.null`.
#' @param row Select only specify row(s). Useful for debugging
#' @inheritParams do_geo_recode
#' @aliases make_file lag_fil
#' @importFrom data.table `:=` `%chin%`
#' @importFrom crayon `%+%`
#' @family filegroups functions
#' @export
make_file <- function(group = NULL,
                      koblid = NULL,
                      aggregate = getOption("orgdata.aggregate"),
                      save = FALSE,
                      year = NULL,
                      implicitnull = getOption("orgdata.implicit.null"),
                      row = getOption("orgdata.debug.row"),
                      base = getOption("orgdata.recode.base")
                      ) {

  LEVEL <- NULL

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

    is_verbose(msg = is_line_long(), type = "other")
    is_verbose(fileSpec$KOBLID, "KOBLID:")

    fileCtrl <- fileSpec[["KONTROLLERT"]] #if file has been checked for error

    dt <- is_org_process(
      file = filePath,
      filespec = fileSpec,
      fgspec = fgSpec,
      con = kh$dbconn,
      row = row,
      control = fileCtrl
    )

    ## RESHAPE structure -----------------------------------------
    reshVal <- find_column_input(fileSpec, "RESHAPE")
    reshapeLong <- reshVal == 1
    reshapeWide <- reshVal == 2

    ## Rename columns "variable" and "value" back as TAB1 to 3 and VAL1 to 3 as
    ## defined in Access coz aggregating uses the standard columnames. Else it
    ## will be deleted as undefined columns in Access database
    if (!is.na(reshVal) && reshapeLong){
      dt <- do_reshape_rename_col(dt = dt, spec = fileSpec)
    }

    ## Recode must happen before reshape wide as reshape wide will use selected TAB
    ## of reshape column creating columns of unique value of TAB ie. wideCols object
    dt <- do_recode(dt = dt, spec = fileSpec, con = kh$dbconn, control = fileCtrl)
    dt <- do_recode_regexp(dt = dt, spec = fileSpec, con = kh$dbconn)

    ## Reshape to wide needs to keep object wideCols from original file
    ## to reshape it back to long if it's wide and became TAB
    wideCols <- NULL
    if (!is.na(reshVal) && reshapeWide){
      meltSpec <- get_reshape_wide_spec(dt, spec = fileSpec)
      resCol <- meltSpec$rescol
      resVal <- meltSpec$resval
      wideCols <- meltSpec$widecols
    }

    ## Only columns defined in tbl_Filgruppe will be kept. Deleting columns only
    ## after renaming RESHAPE columns back to standard columnames.
    deleteVar <- setdiff(names(dt), dataCols)

    if (length(deleteVar) != 0) {
      dt[, (deleteVar) := NULL]
    }

    if (length(deleteVar) != 0) {
      ## What does this mean? Need to ask the senior people in the project :-)
      msg01 <- "Are you sure the deleted column(s) doesn't contain subtotal?"
      msg02 <- "Else aggregating will be incorrect. Define it in FILGRUPPE and delete later"
      msgWarn <- paste0(msg01, "\n", msg02)
      is_verbose(x = msgWarn, type = "warn", ctrl = fileCtrl)
      deleteVar <- paste(deleteVar, collapse = ", ")
      is_verbose(x = paste_cols(deleteVar), "Deleted column(s):", type = "warn2", ctrl = fileCtrl)
    }

    ## RESAHPE WIDE only after undefined column(s) are deleted. Else needs to
    ## make specification for column that should not be included in the formula
    ## LHS ~ RHS. TODO The function to exclude the column is not implemented yet.
    if (!is.na(reshVal) && reshapeWide){
      dt <- do_reshape_wide(dt, meltSpec)
      wideCols <- intersect(names(dt), wideCols)
    }

    ## AGGREGATE ------------------------------------
    ## is_verbose(msg = is_line_short(), type = "other", ctrl = FALSE)

    ## TODO - Not sure if this necessary. Turn of temporarily
    ## Convert some columns to interger. Must be after
    ## the variables are recoded eg. INNKAT is string before recorded to number
    ## dt <- is_col_int(dt)

    dt <- is_aggregate(dt = dt,
                       fgspec = fgSpec,
                       year = year,
                       aggregate = aggregate,
                       base = base,
                       control = fileCtrl,
                       wide = wideCols)


    ## RESHAPE LONG SPECIAL CASES --------------------------------------
    if (!is.na(reshVal) && reshapeWide){
      idvar <- setdiff(names(dt), wideCols)
      dt <- do_reshape_long(dt = dt, resval = resVal, rescol = resCol, widecols = wideCols)
    }

    DT[[i]] <- copy(dt)
    rm(dt)
    gc()
  }

  ## PROCESS ON FILGRUPPE LEVEL ----------------------------------
  outDT <- data.table::rbindlist(DT, fill = TRUE)
  rm(DT)

  if (getOption("orgdata.debug.geo")){
    return(outDT)
  }

  geoLevel <- get_aggregate(spec = fgSpec)

  is_verbose(msg = is_line_short(), type = "other", ctrl = FALSE)

  if (implicitnull){
    for(gg in geoLevel){
      dtsub <- outDT[LEVEL == gg]
      dnull <- do_implicit_null(dtsub, level = gg)
      if (nrow(dnull) > 0){
        impMsg <- sprintf("Number of row(s) with implicit null for %s:", gg)
        is_verbose(x = nrow(dnull), msg = impMsg)
        outDT <- data.table::rbindlist(list(outDT, dnull))
      }
    }
  }

  data.table::setkeyv(outDT, c("GEO", "AAR"))

  is_verbose(msg = is_line_short(), type = "other", ctrl = FALSE)
  outDT <- do_recode_aggregate(dt = outDT,
                               spec = fileSpec,
                               con = kh$dbconn,
                               control = fileCtrl)

  standardCols <- is_standard_cols()
  orderCols <- intersect(standardCols, names(outDT))
  data.table::setcolorder(outDT, orderCols)

  grpCols <- get_colname(spec = fgSpec)
  outDT <- do_colname(dt = outDT, cols = grpCols)

  ## DELETE OLD BYDEL ---------------------------
  bySpec <- get_extra_args_group(spec = fgSpec)
  outDT <- do_extra_args_group(dt = outDT, args = bySpec )


  if (save) {
    save_file(dt = outDT, name = group, fgSpec = fgSpec)
  }

  return(outDT[])
}

#' @export
#' @rdname make_file
lag_fil <- make_file


## Helper -----------------------------------------
## Helper functions are in file utils-read-org.R
