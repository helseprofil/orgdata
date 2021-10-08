## Helper functions mainly for make_file
## -------------------------------------

## Create complete path to DB file
is_path_db <- function(db, check = FALSE) {
  ## db - Database file
  db <- file.path(
    getOption("orgdata.drive"),
    getOption("orgdata.folder.db"),
    db
  )

  if (isTRUE(check) && isFALSE(file.exists(db))) {
    is_stop("Access database file does not exist! \n", var = db)

  }

  return(db)
}

## Get columnames to be kept
is_data_cols <- function(fgspec = NULL){
  stdCols <- getOption("orgdata.columns")
  vars <- list()

  newCols <- get_colname(spec = fgspec)
  if (length(newCols) == 2){
    vars$new <- newCols$new
  }

  splitCols <- get_split(spec = fgspec)
  if (length(splitCols) == 2){
    vars$to <- splitCols$to
  }
  varCols <- unname(unlist(vars))

  if (length(varCols) == 1 && is.na(varCols)) {
    stdCols
  } else {
    c(stdCols, varCols)
  }
}



is_aggregate <- function(dt, fgspec, verbose = getOption("orgdata.verbose"), year = year){

  is_verbose("Starts aggregating data ...")
  aggSpec <- get_aggregate(spec = fgspec)
  source <- is_geo_level(dt$GEO[1])

  nSpec <- length(aggSpec)
  DT <- vector(mode = "list", length = nSpec)
  for (i in seq_len(nSpec)) {
    dtt <- do_aggregate(dt = dt, source = source, level = aggSpec[i], year = year)
    dtt <- do_aggregate_recode(dt = dtt)
    DT[[i]] <- data.table::copy(dtt)
    gc()
  }
  rm(dtt)
  data.table::rbindlist(DT, use.names = TRUE, fill = TRUE)
}

## identify geo level
is_geo_level <- function(x){
  geo <- nchar(x)
  data.table::fcase(geo %in% 7:8, "grunnkrets",
                    geo %in% 5:6, "bydel",
                    geo %in% 3:4, "kommune",
                    geo %in% 1:2, "fylke")

}


## Create complete path to raw data file
is_path_raw <- function(spec, check = FALSE) {
  filename <- find_column_input(spec, "FILNAVN")
  filePath <- file.path(getOption("orgdata.folder.data"), filename)

  if (isTRUE(check) && isFALSE(file.exists(filePath))) {
    is_stop("File does not exist! \n", filePath)
  }

  return(filePath)
}

## Exclude files after KOBLID and IBRUKTIL
is_org_files <- function(spec, id = NULL) {
  IBRUKTIL <- KOBLID <- NULL

  koblid <- spec$KOBLID

  ## TODO Implement spec as DT from parent.env
  data.table::setDT(spec)
  if (!is.null(id)) {
    koblid <- id
    spec <- spec[KOBLID %in% koblid, ]
  }

  spec[, IBRUKTIL := as.Date(IBRUKTIL, format = "%Y-%m-%d")]
  spec <- spec[IBRUKTIL == as.Date("9999-01-01", format = "%Y-%m-%d"), ]

  nfile <- nrow(spec)
  if (nfile == 0) {
    is_stop("No valid file found!")
  }

  data.table::setDF(spec)
}


## Covert to integer for columns integer but only after
## variables are recoded
is_col_int <- function(dt){
  cols <- getOption("orgdata.int")

  ints <- c("UTDANN", "SIVILSTAND", "LANDB", "LANDF", "INNKAT")
  noExtra <- setdiff(names(dt), ints)
  extraInts <- names(dt)[!(names(dt) %in% noExtra)]

  colsInt <- c(cols, extraInts)

  for (j in seq_len(length(colsInt))){
    col <- colsInt[j]
    if (methods::is(dt[[col]], "character")) {
      data.table::set(dt, j = col, value = as.integer(dt[[col]]))
    }
  }
  return(dt)
}
