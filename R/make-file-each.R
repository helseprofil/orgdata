#' @title Process Each File
#' @description Process each file parallelly
#' @param i File specification
#' @param spec File specification
#' @param fgspec Filegroup specification
#' @param datacols Columnames to be kept
#' @inheritParams make_file
#' @export
do_make_file_each <- function(i, spec, fgspec, aggregate, datacols, year, row, base){
  fileSpec <- spec[i, ]
  filePath <- is_path_raw(fileSpec, check = TRUE)

  is_verbose(msg = is_line_long(), type = "other")
  is_verbose(fileSpec$KOBLID, "KOBLID:")

  fileCtrl <- get_column_input(fileSpec, "KONTROLLERT")
  koblID <- get_column_input(fileSpec, "KOBLID")

  DB <- is_conn_db(db = "kh")

  dt <- is_org_process(
    file = filePath,
    filespec = fileSpec,
    fgspec = fgspec,
    con = DB$dbconn,
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
  dt <- do_recode(dt = dt, spec = fileSpec, con = DB$dbconn, control = fileCtrl)
  dt <- do_recode_regexp(dt = dt, spec = fileSpec, con = DB$dbconn)

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
  deleteVar <- setdiff(names(dt), datacols)

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
                     fgspec = fgspec,
                     year = year,
                     aggregate = aggregate,
                     base = base,
                     control = fileCtrl,
                     wide = wideCols,
                     koblid = koblID)


  ## RESHAPE LONG SPECIAL CASES --------------------------------------
  if (!is.na(reshVal) && reshapeWide){
    idvar <- setdiff(names(dt), wideCols)
    dt <- do_reshape_long(dt = dt, resval = resVal, rescol = resCol, widecols = wideCols)
  }

  data.table::copy(dt)
}