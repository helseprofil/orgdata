## Helper functions mainly for make_file
## -------------------------------------

## Create complete path to DB file
is_path_db <- function(db, check = FALSE) {
  ## db - Database file
  db <- file.path(
    os_drive(),
    getOption("orgdata.folder.db"),
    db
  )

  if (isTRUE(check) && isFALSE(file.exists(db))) {
    is_stop("Database file not found! \n", var = db)
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
                         verbose = NULL,
                         year = NULL,
                         aggregate = NULL,
                         base = NULL,
                         control = FALSE,
                         wide = NULL, ...){

  is_debug(deep = TRUE)

  GEO <- NULL

  if (is.null(verbose)) verbose <- getOption("orgdata.verbose")
  if (is.null(year)) year <- getOption("orgdata.year")
  if (is.null(aggregate)) aggregate <- getOption("orgdata.aggregate")
  if (is.null(base)) base <- getOption("orgdata.recode.base")

  aggSpec <- get_aggregate(spec = fgspec)
  source <- is_geo_level(dd = dt)
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
is_geo_level <- function(x = NULL, dd = NULL){

  if (!is.null(dd)){
    x <- is_digit_geo(dd)
  }

  geo <- nchar(x)
  data.table::fcase(geo %in% 7:8, "grunnkrets",
                    geo %in% 5:6, "bydel",
                    geo %in% 3:4, "kommune",
                    geo %in% 1:2, "fylke")

}


## Create complete path to raw data file
is_path_raw <- function(spec, check = FALSE) {
  filename <- find_column_input(spec, "FILNAVN")
  filePath <- file.path(os_drive(), getOption("orgdata.folder.data"), filename)

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

  ## Second filter date after SQL filter
  ## To fix with testthat error
  today <- format(Sys.Date(), "%Y-%m-%d")
  spec <- spec[IBRUKTIL >= today, ]

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

# Ensure the maximum digits in the dataset to identify geo level
is_digit_geo <- function(dd, .test = FALSE){

  if (.test){
    set.seed(1472)
  }

  GEO <- digitGEO <- NULL
  dt <- dd[!is.na(GEO) | GEO!=""][sample(1:.N, 50)]
  dt[, digitGEO := nchar(GEO)]
  geo <- dt[max(digitGEO), list(GEO)]
  return(geo)
}

# Select files using vectors instead of KOBLID
is_select_file <- function(spec, select, rowfile){

  if (!is.null(select)){
    if (is.element("last", select)){
      select <- max(nrow(spec))
    }

    spec <- spec[select,]
    rowfile <- nrow(spec)
    is_color_txt(length(select), "Number of file(s) selected:")
  }

  return(list(spec = spec, rowFile = rowfile))
}

# Filegroup name
# y - selected filegroup
# con - connection
is_filegroup <- function(y, con){
  gp <- find_spec("filegroup-all.sql",
                  con = con,
                 asis = TRUE)

  x <- gp$FILGRUPPE

  if(any(y == x)){
    return(y)
  } else {
    g <- substr(y, 1, 2)
    gg <- grep(paste0("^", g), x, value = TRUE)
  }

  is_stop("Can't find filegroup. May be you mean one of these? \n", gg)
  invisible()
}
