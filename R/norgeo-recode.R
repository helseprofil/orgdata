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

  if (type == "grunnkrets"){
    dt <- is_grunnkrets(dt)
    dt <- is_grunnkrets_na(dt)
    dt <- is_grunnkrets_0000(dt)
    dt <- is_grunnkrets_before_2002(dt, year, con)
  }

  data.table::setkey(dt, GEO)

  xcode <- is_warn_geo_merge(dt, code, vector = FALSE)
  xind <- dt[, .I[GEO %in% xcode]]
  dt <- is_delete_index(dt, xind) #delete row that can't be merged

  ## Select the first code for split code
  codeGeo <- c("GEO", "to")
  data.table::setkeyv(code, codeGeo)
  code <- code[!duplicated(GEO) | duplicated(GEO, fromLast = TRUE)]

  if (geo){
    is_debug_warn("`orgdata.debug.geo`")
    dt[code, on = "GEO", "geo2" := i.to]
    geoVar <- c("rawGEO", "GEO")
    data.table::setnames(dt, c("GEO", "geo2"), geoVar)
    data.table::setcolorder(dt, c(geoVar, "AAR"))
    dt[, "dummy_grk" := NULL]
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
    idx <- dt[, .I[is.na(GEO)]]
    idx <- is_check_geo(idx)

    dt[is.na(GEO), GEO := 99999999]
    is_colour_txt(x = nrNA, msg = "Number of missing GEO with empty value or NA:", type = "warn2")
    is_colour_txt(x = 99999999, msg = "Missing GEO are now recoded to", type = "note")
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

    for (i in idx){
      code <- sub("0{4}$", "", dt[i]$GEO)
      grc <- paste0(code, "9999")
      dt[i, GEO := as.integer(grc)]
    }

    is_colour_txt(x = nr00, msg = "Number of GEO codes inconsistence with geo coding:", type = "warn2")
    idx <- is_check_geo(idx)
    is_colour_txt(x = "xxxx9999", msg = "They are now recoded with ending:", type = "note")
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

  is_verbose(length(idx), "Number of GEO codes need to be checked:", type = "warn2")
  idx <- is_check_geo(idx)
  is_verbose(msg = "99 or 9999 are added to the end of the code respectively")

  for (i in idx){

    val <- dt[i, dummy_grk]
    addVal <- is_geo_oddeven(val)
    val9 <- as.integer(paste(rep(9, addVal), collapse = ""))

    dt[i, GEO := as.integer(paste0(GEO, val9))]
  }

  dt[, dummy_grk := NULL]
}

## Grunnkrets codes for change starts from 2002. All others before that need to
## have dummy from municipality to be able to recode to current geo code
is_grunnkrets_before_2002 <- function(dt, year, con){
  GEO <- Geo_Dummy <- GEO_New <- NULL
  yr <- unique(dt$AAR)

  yrOld <- is.element(yr, 1980:2001)

  if (yrOld){
    geoTable <- paste0("kommune", year)
    geoDT <- find_spec("geo-recode-dummy.sql", char = geoTable, con = con, char2 = 2002)
    data.table::setDT(geoDT)

    oldCodes <- as.integer(unique(geoDT$oldCode))
    dt[ , Geo_Dummy := sub("\\d{4}$", "", GEO) ]
    dt[, GEO_New := data.table::fcase(Geo_Dummy %in% oldCodes, sub("\\d{4}$", "9999", GEO),
                                      default = NA)]
    dt[!is.na(GEO_New), GEO := as.integer(GEO_New)]
    dt[, c("Geo_Dummy", "GEO_New") := NULL]
  }
  return(dt)
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
is_check_geo <- function(idx){
  ## idx - Row index
  idxNo <- is_short_code(idx, n1 = 10, n2 = 7)
  ## is_verbose(msg = is_line_short(), type = "other")
  is_verbose(idxNo, "Check GEO codes in original data for row(s):", type = "warn")
  invisible(idx)
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
    ## is_verbose(msg = is_line_short(), type = "other")
    is_verbose(x = length(dcode), msg = "Number of geo codes fail to recode and are excluded:", type = "warn2")
    is_verbose(x = codes, msg = "These are the codes:")
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
