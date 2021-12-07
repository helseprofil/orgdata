#' @title Recode Geo Code Without Aggregate
#' @description Recode geo code without aggregating the data.
#'   The input in argument `source` must be a lower
#'   granularity level than the `level` input.
#' @inheritParams do_split
#' @param source What geographical granularity codes that is available in the
#'   source data. This will be used for merging with the output from
#'   `geo_levels()`
#' @param year Which year the georaphical code is valid for. If not specified,
#'   then it will be base on the year in source data ie. column `AAR`
#' @param check If TRUE then output will keep variables for geographical levels
#'   without aggregating it. This is useful to check for geographical codes that
#'   are missing. Else use `options(orgdata.aggregate = FALSE)`
#' @examples
#' \dontrun{
#' # Source data with enumeration area codes ie. grunnkrets
#' dt <- make_file("BEFOLKNING", aggregate = FALSE)
#' }
#' @import data.table
#' @family geo recode functions
#' @export
do_recode_without_aggregate <- function(dt = NULL,
                                        source = c(
                                          "grunnkrets",
                                          "fylke",
                                          "kommune",
                                          "bydel"
                                        ),
                                        year = NULL,
                                        check = getOption("orgdata.debug.aggregate")){
  AAR <- NULL
  cat("..")

  is_debug()
  is_null(dt)
  dt <- data.table::copy(dt)

  source <- tolower(source)
  source <- match.arg(source)

  geoFile <- is_path_db(getOption("orgdata.geo"), check = TRUE)
  geoDB <- is_conn_db(geoFile)

  cat("..")
  ## validTo in the database `tblGeo` is a character
  if (!is.null(year)) {
    yr <- dt[AAR == year, ][1]
  } else {
    yr <- as.integer(format(Sys.Date(), "%Y"))
  }

  ## recode GEO codes
  code <- get_geo_recode(con = geoDB$dbconn, type = source, year = yr)
  cat("..\n")
  dt <- do_geo_recode(dt = dt, code = code, type = source, year = yr, con = geoDB$dbconn)
}

#' @title Recode Geographical Codes
#' @description Recode geographical codes to the current year. Codes is based on
#'   [norgeo::track_change()] function. For a split geogaphical codes from
#'   previous year, the first code of the current year code in chronological order
#'   will be selected to recode.
#' @inheritParams do_split
#' @param code Code dataset of old and new codes in a `data.table` format.
#' @param type The geographical granularity for recoding The dataset is the
#'   output after running `get_geo_recode()` function.
#' @param year Which year the geograhical codes to be recoded to. If it is empty
#'   then current year will be used.
#' @inheritParams find_spec
#' @param geo Keep old geographical code if TRUE. Default is FALSE.
#' @examples
#' \dontrun{
#' code <- get_geo_recode(con = geo$dbconn, type = "grunnkrets")
#' dt <- make_file("BEFOLKNING", aggregate = FALSE)
#' DT <- do_geo_recode(dt, code)
#' }
#' @import data.table
#' @family geo recode functions
#' @export
do_geo_recode <- function(dt = NULL,
                          code = NULL,
                          type = c(
                            "grunnkrets",
                            "fylke",
                            "kommune",
                            "bydel"),
                          year = NULL,
                          con = NULL,
                          geo = getOption("orgdata.debug.geo")
                          ){
  GEO <- i.to <- NULL

  dt <- data.table::copy(dt)

  ## Ensure variables to be used to aggregate in type int
  intVar <- c("GEO", "VAL1")
  for (col in intVar){
    if (is(dt[[col]], "character"))
      data.table::set(dt, j = col, value = as.integer(dt[[col]]))
  }

  # keep original code for debug.geo
  dt[, "origin" := GEO]

  if (type == "grunnkrets"){
    dt <- is_grunnkrets(dt)
    dt <- is_grunnkrets_na(dt)
    dt <- is_grunnkrets_0000(dt)
  }

  data.table::setkey(dt, GEO)

  xcode <- is_warn_geo_merge(dt, code, vector = FALSE)
  xind <- dt[, .I[GEO %in% xcode]]
  dt <- is_delete_index(dt, xind) #delete row that can't be merged

  ## Use the first code to recode to new geo codes if old geo codes were split
  ## into multiple new codes
  code <- code[!duplicated(GEO)][!is.na(GEO)]

  if (geo){
    is_debug_warn("`orgdata.debug.geo`")
    dt[code, on = "GEO", "geo2" := i.to]
    dt[, c("GEO", "dummy_grk") := NULL]
    geoVar <- c("oriGEO", "GEO")
    data.table::setnames(dt, c("origin", "geo2"), geoVar)
    data.table::setcolorder(dt, c(geoVar, "AAR"))
  } else {
    dt[code, on = "GEO", GEO := i.to]
  }
}

#' @title Get Previous and Current Geo Codes
#' @description Get the geographical codes registered in `geo-database` which consist
#'  of old and new codes that are applicable to the respective year.
#' @inheritParams find_spec
#' @inheritParams do_geo_recode
#' @return A dataset with columns `GEO` and `to` representing the GEO
#'  codes that will be recoded to a new code ie. `to`.
#' @import data.table
#' @family geo recode functions
#' @export
get_geo_recode <- function(con = NULL,
                           type = c(
                             "grunnkrets",
                             "fylke",
                             "kommune",
                             "bydel"),
                           year = NULL){

  changeOccurred <- NULL

  is_debug()
  is_null(con)
  is_null(year)
  type <- match.arg(type)

  if (is.null(year)) {
    year <- as.integer(format(Sys.Date(), "%Y"))
  }

  geoTable <- paste0(type, year)
  geoDT <- find_spec("geo-recode.sql", value = geoTable, con = con)
  data.table::setDT(geoDT)

  for (j in seq_len(ncol(geoDT))){
    if (class(geoDT[[j]]) == 'character')
      data.table::set(geoDT, j = j, value = as.integer(geoDT[[j]]))
  }

  geoDT[, changeOccurred := NULL]
  geoCols <- c("GEO", "to")
  data.table::setnames(geoDT, c("oldCode", "currentCode"), geoCols)
  data.table::setkeyv(geoDT, geoCols)
}

## Helper -----------------
is_grunnkrets_na <- function(dt){
  GEO <- AAR <- NULL

  nrNA <- dt[is.na(GEO), .N]
  if (nrNA > 0){
    dt[is.na(GEO), GEO := 99999999]
    is_verbose(x = nrNA, msg = "Number of missing GEO with empty value or NA:", type = "warn2")
    is_verbose(x = 99999999, msg = "Missing GEO are now recoded to", type = "note")
    is_verbose(x = "`df[is.na(geoColName),]`", msg = "Check in the original data with", type = "note")
  }

  return(dt)
}


## Convert geo ends with 4 zeros ie. xxxx0000 to xxxx9999
## Can't aggregate grunnkrets ends with 4 zeros or 2 zeros as it only represents delområde
is_grunnkrets_0000 <- function(dt){
  GEO <- AAR <- NULL

  nr00 <- dt[GEO %like% "0000$", .N]
  if (nr00 > 0){
    idx <- dt[, .I[GEO %like% "0000$"]]
    notCodes <- dt[idx]$GEO

    for (i in idx){
      code <- sub("0{4}$", "", dt[i]$GEO)
      grc <- paste0(code, "9999")
      dt[i, GEO := as.integer(grc)]
    }

    is_log(notCodes, "code00")
    is_verbose(x = nr00, msg = "Number of GEO codes inconsistence with geo coding:", type = "warn2")
    is_check_geo(notCodes)
    is_verbose(x = "xxxx9999", msg = "They are now recoded with ending:", type = "note")
    is_verbose(x = "log$code00", msg = "To see these codes, run command:")
  }

  return(dt)
}


## Some grunnkrets have less than 7 digits but not missing. This will add 99 or
## 9999 to these number accrodingly making grunnkrets standard with 8 or 7 digits.
is_grunnkrets <- function(dt){
  GEO <- dummy_grk <- NULL

  dt[, dummy_grk := data.table::fifelse(nchar(GEO) > 6 , yes = 0, no = 1, na = 0)]

  dummy <- dt[dummy_grk != 0, .N]
  if (dummy == 0){
    return(dt)
  }

  dt[dummy_grk != 0 , dummy_grk := nchar(GEO)]
  idx <- dt[, .I[dummy_grk != 0]]
  notCodes <- dt[idx]$GEO

  is_log(value = notCodes, x = "codeShort")
  is_verbose(length(idx), "Number of GEO codes need to be checked:", type = "warn2")
  is_check_geo(notCodes)
  is_verbose(msg = "99 or 9999 are added to the end of the code respectively")
  is_verbose(x = "log$codeShort", msg = "To see these codes, run command:")

  for (i in idx){

    val <- dt[i, dummy_grk]
    addVal <- is_geo_oddeven(val)
    val9 <- as.integer(paste(rep(9, addVal), collapse = ""))

    dt[i, GEO := as.integer(paste0(GEO, val9))]
  }

  dt[, dummy_grk := NULL]
}

## Grunnkrets have btw 7 to 8 digits only
is_geo_oddeven <- function(x){

  oddNo <- identical(x %% 2, 1)
  if (oddNo){
    7 - x
  } else {
    8 - x
  }
}

## Don't overflood the console!
is_check_geo <- function(codes){
  ## codes - Codes to display
  codes <- data.table::copy(codes)
  codesNot <- is_short_code(codes, n1 = 10, n2 = 8)
  ## is_verbose(msg = is_line_short(), type = "other")
  is_verbose(codesNot, "Check GEO codes in original data:", type = "warn")
  invisible(codes)
}

## Codes that can't be merged since it's not found in geo codebook database
is_warn_geo_merge <- function(x, y, vector = FALSE){
  ## x - dataset
  ## y - geocodes
  ## vector - Either a data.frame or vector
  GEO <- to <- NULL

  if (vector){
    x <- unique(x[!is.na(x)])
    y <- unique(y[!is.na(y)])
  } else {
    dtc <- unique(x[!is.na(GEO)]$GEO)
    x <- setdiff(dtc, unique(y[!is.na(to)]$to))
    y <- unique(y[!is.na(GEO)]$GEO)
  }

  dcode <- setdiff(x, y)
  if (length(dcode) > 0){
    codes <- is_short_code(dcode, n1 = 10, n2 = 8)
    is_log(value = dcode, x = "codeDelete")
    is_verbose(x = length(dcode), msg = "Number of geo codes fail to recode and are excluded:", type = "warn2")
    is_verbose(x = codes, msg = "These are the codes:")
    is_verbose(x = "log$codeDelete", msg = "To see these codes, run command:")
  }

  return(dcode)
}


is_short_code <- function(x, n1 = 10, n2 = 6){
  ## n1 - maximum length before making cutoff
  ## n2 - maximum codes to display
  if (length(x) > n1){
    codes <- c(x[1:n2], "...")
  } else {
    codes <- x
  }

  paste_cols(codes)
}

