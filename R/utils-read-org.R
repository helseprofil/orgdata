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

  stdCols <- is_standard_cols()
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


is_aggregate <- function(dt = NULL,
                         fgspec = NULL,
                         verbose = getOption("orgdata.verbose"),
                         year = getOption("orgdata.year"),
                         aggregate = getOption("orgdata.aggregate"),
                         base = getOption("orgdata.recode.base"),
                         control = FALSE,
                         wide = NULL, ...){

  is_debug(deep = TRUE)

  GEO <- NULL

  aggSpec <- get_aggregate(spec = fgspec)
  source <- is_geo_level(dt[!is.na(GEO), GEO][1])
  aggCol <- find_column_multi(spec = fgspec, "AGGKOL") #Other columns to aggregate

  geoFile <- is_path_db(getOption("orgdata.geo"), check = TRUE)
  geoDB <- is_conn_db(geoFile)

  ## validTo in the database `tblGeo` is a character

  ## Ensure variables to be used to aggregate is type numeric
  colVals <- paste0("VAL", 1:getOption("orgdata.vals"))
  dt <- is_col_num(dt = dt, cols = colVals)

  ## recode GEO codes
  code <- get_geo_recode(con = geoDB$dbconn, type = source, year = year)
  dt <- do_geo_recode(dt = dt,
                      code = code,
                      type = source,
                      year = year,
                      con = geoDB$dbconn,
                      base = base,
                      control = control, ...)

  is_verbose(msg = is_line_short(), type = "other", ctrl = FALSE)

  if (getOption("orgdata.debug.geo")){
    return(dt)
  }

  if (aggregate){
    geoCode <- is_geo_cast(source = source, year = year)
    nSpec <- length(aggSpec)

    DT <- lapply(seq_len(nSpec),
                 function(x){
                   do_aggregate(
                     dt = dt,
                     source = source,
                     level = aggSpec[x],
                     year = year,
                     aggregate.col = aggCol,
                     geoDT = geoCode,
                     base = base,
                     control = control,
                     wide = wide)
                 })

    dt <- data.table::rbindlist(DT, use.names = TRUE, fill = TRUE)
  } else {
    is_verbose(x = "", msg = "Dataset will not be aggregated!", type = "warn")
    dt <- do_recode_without_aggregate(dt = dt,
                                      source = source,
                                      year = year)
  }

  return(dt)
}

## identify geo level base on number of digits in codes
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


## Covert to numeric for columns that are expected to be numeric
## but only after variables are recoded
is_col_num <- function(dt, cols){

  for (j in seq_len(length(cols))){
    col <- cols[j]
    if (methods::is(dt[[col]], "character")) {
      data.table::set(dt, j = col, value = as.numeric(dt[[col]]))
    }
  }
  return(dt)
}
