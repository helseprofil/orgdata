#' @title Process Each File
#' @description Process each file according to the specification in Access database.
#' @param spec File specification
#' @param fgspec Filegroup specification
#' @param datacols Columnames to be kept
#' @inheritParams make_file
#' @param duck R6 object for DuckDB
#' @export
do_make_file_each <- function(spec, fgspec, aggregate, datacols, year, row, base, duck = NULL){
  is_debug()
  fileSpec <- spec
  filePath <- is_path_raw(fileSpec, check = TRUE)
  filePath <- gsub("\\\\", "/", filePath)

  is_verbose(msg = is_line_long(), type = "other")

  koblID <- find_column_input(fileSpec, "KOBLID")
  is_verbose(koblID, "KOBLID:")

  debugOpt <- is_option_active()
  if (!debugOpt){
    fileCtrl <- find_column_input(fileSpec, "KONTROLLERT")
  } else {
    fileCtrl <- FALSE
  }

  DB <- is_conn_db(db = "kh")

  ## Check dataset in DuckDB -------------
  duckIDs <- DBI::dbListTables(duck$dbconn)
  duckTbl <- is_tables_name(koblID)
  fileDuck <- any(duckTbl %chin% duckIDs)

  ## Read from raw file if not allready found in DuckDB
  if (!fileCtrl || !fileDuck) {
    dt <- is_process_file(
      file = filePath,
      filespec = fileSpec,
      fgspec = fgspec,
      con = DB$dbconn,
      row = row,
      control = fileCtrl,
      duck = duck
    )

    ## RESHAPE structure -----------------------------------------
    reshVal <- find_column_input(fileSpec, "RESHAPE")
    reshapeLong <- reshVal == 1
    reshapeWide <- reshVal == 2

    ## Rename columns "variable" and "value" back to TAB1..TAB3 and VAL1..VAL3 as
    ## defined in Access coz aggregating uses the standard columnames. Else it
    ## will be deleted as undefined columns in Access database
    if (!is.na(reshVal) && reshapeLong)
      dt <- do_reshape_rename_col(dt = dt, spec = fileSpec)

    ## Recode must happen before reshape wide coz reshape wide will use selected TAB
    ## of reshaped column, creating columns of unique value of TAB ie. wideCol object
    dt <- do_recode(dt = dt, spec = fileSpec, con = DB$dbconn, control = fileCtrl)
    dt <- do_recode_regexp(dt = dt, spec = fileSpec, con = DB$dbconn)

    ## Reshape to wide needs to keep object wideCol from original file
    ## to reshape it back to long if it's wide and became TAB
    wideCol <- NULL
    if (!is.na(reshVal) && reshapeWide){
      wideSpec <- get_reshape_wide_spec(dt, spec = fileSpec)
      wideCol <- wideSpec$widecol
    }

    ## DELETE UNDEFINED COLUMNS ------------------------
    ## Only columns defined in tbl_Filgruppe will be kept. Deleting columns only
    ## after renaming specified RESHAPE columns back to standard columnames.
    deleteVar <- setdiff(names(dt), datacols)

    if (length(deleteVar) != 0)
      dt[, (deleteVar) := NULL]


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
      dt <- do_reshape_wide(dt, wideSpec)
      wideCol <- intersect(names(dt), wideCol)
    }

    ## AGGREGATE ------------------------------------
    ## TODO - Not sure if this's necessary. Turn off temporarily
    ## Convert some columns to interger. Must be after
    ## the variables are recoded eg. INNKAT is string before recorded to numeric
    ## dt <- is_col_num(dt)

    dt <- is_aggregate(dt = dt,
                       fgspec = fgspec,
                       year = year,
                       aggregate = aggregate,
                       base = base,
                       control = fileCtrl,
                       wide = wideCol,
                       koblid = koblID)


    ## RESHAPE LONG SPECIAL CASES --------------------------------------
    ## When dataset is long then reshape to wide before long again
    if (!is.na(reshVal) && reshapeWide){
      dt <- do_reshape_long(dt = dt, respec = wideSpec)
      dt <- is_long_col(dt, spec = fileSpec, widespec = wideSpec)
    }
  }

  ## When debug then skip everything with DuckDB
  if (debugOpt)
    return(data.table::copy(dt))


  ## Add to or read from DuckDB -------------
  fileName <- find_column_input(fileSpec, "FILNAVN")
  fileName <- paste0("../", gsub("\\\\", "/", fileName))

  if (!fileCtrl && fileDuck){
    opt <- "option1"
    optMsg <- "Delete dataset in the data warehouse ..."
  } else if (fileCtrl && !fileDuck){
    opt <- "option2"
    optMsg <- "Adding dataset to data warehouse ..."
  } else if (fileCtrl && fileDuck){
    opt <- "option3"
    optMsg <- "Data found in data warehouse! To read from raw data uncheck KONTROLLERT or use `raw=TRUE` instead"
  }

  is_verbose(msg = is_line_short(), type = "other", ctrl = FALSE)
  withr::with_options(list(orgdata.emoji = "safe"),
                      is_color_txt(x = "", msg = optMsg, type = "debug", emoji = TRUE))

  switch(opt,
         option1 = duck$db_remove_table(name = duckTbl),
         option2 = duck$db_write(name = duckTbl, value = dt, write = TRUE),
         option3 = {
           is_color_txt(x = fileName, msg = "File:")
           dt <- duck$db_read(name = duckTbl)
         })

  data.table::copy(dt)
}


## HELPER ---------------------------
# Delete rows created from cross join that shouldn't be there
is_long_col <- function(dt, spec, widespec){
  # spec - file specification
  # widespec - Spec for reshape wide
  delTabs <- NULL
  mulTabs <- is_multi_tabs(spec)
  if (length(mulTabs) > 1){
    dt[, "delTabs" := do.call(paste0, .SD), .SDcols = mulTabs]
    ## Delete extra rows that aren't found in original data but
    ## were created when aggregating the dimensions
    mulIdx <- dt[!(delTabs %chin% widespec$multtab), which = TRUE]
    dt <- is_delete_index(dt, mulIdx)
    dt[, "delTabs" := NULL]
  }

  return(dt)
}

