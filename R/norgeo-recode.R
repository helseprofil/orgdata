#' @title Recode Geo Code Without Aggregate
#' @description Recode geo code without aggregating the data.
#'   The input in argument `source` must be a lower
#'   granularity level than the `level` input.
#' @inheritParams do_split
#' @param source What geographical granularity codes that is available in the
#'   source data. This will be used for merging with the output from
#'   `geo_map()`
#' @inheritParams do_geo_recode
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
                                        base = NULL,
                                        ...){
  AAR <- NULL
  cat("..")

  if(is.null(year)) year <- getOption("orgdata.year")
  if(is.null(base)) base <- getOption("orgdata.recode.base")

  is_debug()
  is_null(dt)
  dt <- data.table::copy(dt)

  source <- tolower(source)
  source <- match.arg(source)

  geoFile <- is_path_db(getOption("orgdata.geo"), check = TRUE)
  geoDB <- is_conn_db(geoFile)

  cat("..")

  ## recode GEO codes
  code <- get_geo_recode(con = geoDB$dbconn, type = source, year = year)
  cat("..\n")
  dt <- do_geo_recode(dt = dt,
                      code = code,
                      type = source,
                      year = year,
                      con = geoDB$dbconn,
                      base = base, ...)
}

#' @title Recode Geographical Codes
#' @description Recode geographical codes to the current year. Codes is based on
#'   [norgeo::track_change()] function. For a split geogaphical codes from
#'   previous year, the first code of the current year code in chronological order
#'   will be selected to recode.
#' @inheritParams do_split
#' @param code Code dataset of old and new codes in a `data.table` format.
#' @param type The geographical granularity for recoding. The dataset is the
#'   output after running `get_geo_recode()` function.
#' @param year Which year the geograhical codes to be recoded to. If it is empty
#'   then global option for `orgdata.year` will be used.
#' @inheritParams find_spec
#' @param geo Logical value. Keep old geographical code if TRUE. Default is FALSE.
#' @param base Logical value. If `TRUE` then use year in the original data as the base
#'   year to recode the geographical codes. Default is `FALSE` and use all
#'   available codes in geo codebook
#' @param control Logical value. `TRUE` if the file has been controlled for
#'   possible errors
#' @param ... Any additional arguments
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
                          geo = NULL,
                          base = NULL,
                          control = FALSE,
                          ...
                          ){
  GEO <- i.to <- changeOccurred <- NULL

  is_debug()

  if(is.null(year)) year <- getOption("orgdata.year")
  if(is.null(geo)) geo <- getOption("orgdata.debug.geo")
  if(is.null(base)) base <- getOption("orgdata.recode.base")

  withr::with_options(list(orgdata.emoji = "write"),
                      is_color_txt(x = "", msg = "Recode geo codes ...", emoji = TRUE))

  dt <- data.table::copy(dt)

  # keep original code for debug.geo
  dt[, "origin" := GEO]

  ## Ensure GEO can be converted to int and no character GEO
  dt <- is_col_num_warn(dt, "GEO", ...)
  dt[, GEO := as.integer(GEO)]

  if (type == "grunnkrets"){
    dt <- is_grunnkrets(dt, control = control, ...)
    dt <- is_grunnkrets_na(dt, control)
    dt <- is_grunnkrets_0000(dt, control = control, ...)
  }

  data.table::setkey(dt, GEO)

  if (base){
    baseYear <- min(unique(dt$AAR))
    code <- code[changeOccurred >= as.integer(baseYear)]
  }

  code[, changeOccurred := NULL]

  ## recode to unknown grunnkrets if not able to merge ie. xxxx9999
  if (type %in% c("grunnkrets", "bydel")){
    codeProb <- is_problem_geo_merge(dt, code, vector = FALSE, control = control, mode = "recode", ...)
    dt <- is_problem_geo(dt = dt, codes = codeProb, type = type)
    dt <- is_problem_geo_before_2002(dt, codeProb, type = type, year = year, con = con )
  }

  xcode <- is_problem_geo_merge(dt, code, vector = FALSE, control = control, mode = "delete")
  xind <- dt[, .I[GEO %in% xcode]]
  dt <- is_delete_index(dt, xind) #delete row that can't be merged

  ## Use the first code to recode to new geo codes if old geo codes were split
  ## into multiple new codes
  code <- code[!duplicated(GEO)][!is.na(GEO)]

  if (geo){
    is_debug_warn("`orgdata.debug.geo`")

    ## inherit batch number from codebook when debug
    srcBatch <- as.Date(code$batch[1])
    dt[, "batch" := srcBatch]

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
#' @description Get the geographical codes registered in `geo-database` which
#'   consist of old and new codes that are applicable to the respective year.
#' @inheritParams find_spec
#' @param type The geographical granularity to recode
#' @inheritParams do_geo_recode
#' @return A dataset with columns `GEO` and `to` representing the GEO codes that
#'   will be recoded to a new code ie. `to`.
#' @import data.table
#' @family geo recode functions
#' @export
get_geo_recode <- function(con = NULL,
                           type = c(
                             "grunnkrets",
                             "fylke",
                             "kommune",
                             "bydel"),
                           year = NULL
                           ){

  changeOccurred <- NULL

  if (is.null(year)) year <- getOption("orgdata.year")

  is_debug(deep = TRUE)
  is_null(con)
  is_null(year)
  type <- match.arg(type)

  if (is.null(year)) {
    year <- as.integer(format(Sys.Date(), "%Y"))
  }

  geoTable <- paste0(type, year)
  geoDT <- find_spec("geo-recode.sql", value = geoTable, con = con)
  data.table::setDT(geoDT)

  colNames <- c("oldCode", "currentCode", "changeOccurred", "batch")

  for (j in colNames[-4]){
    if (class(geoDT[[j]]) == 'character')
      data.table::set(geoDT, j = j, value = as.integer(geoDT[[j]]))
  }

  ## geoDT[, changeOccurred := NULL]
  geoCols <- c("GEO", "to")
  data.table::setnames(geoDT, colNames[c(1,2)], geoCols)
  data.table::setkeyv(geoDT, geoCols)
}

## Helper -----------------
is_grunnkrets_na <- function(dt, control = FALSE){
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
is_grunnkrets_0000 <- function(dt, control = FALSE, ...){
  GEO <- AAR <- NULL

  is_debug(deep = TRUE)
  nr00 <- dt[GEO %like% "0000$", .N]
  if (nr00 > 0){
    idx <- dt[, .I[GEO %like% "0000$"]]
    notCodes <- dt[idx]$GEO

    dt <- is_replace_geo(dt,
                         idx = idx,
                         from = "0{4}$",
                         to = "9999")

    logCmd <- is_log_write(value = notCodes, x = "code00", ...)
    is_verbose(x = nr00, msg = "Number of GEO codes end with `0000`:", type = "warn2")
    is_check_geo(notCodes, control)
    is_verbose(x = "xxxx9999", msg = "They are now recoded with:", type = "note")
    is_verbose(x = logCmd, msg = "To see these codes, run command:")
  }

  return(dt)
}
## Some grunnkrets have less than 7 digits but not missing. This will add 99 or
## 9999 to these number accrodingly making grunnkrets standard with 8 or 7 digits.
is_grunnkrets <- function(dt, control = FALSE, ...){
  GEO <- dummy_grk <- NULL

  is_debug(deep = TRUE)
  dt[, dummy_grk := data.table::fifelse(nchar(GEO) > 6 , yes = 0, no = 1, na = 0)]

  dummy <- dt[dummy_grk != 0, .N]
  if (dummy == 0){
    return(dt)
  }

  dt[dummy_grk != 0 , dummy_grk := nchar(GEO)]
  idx <- dt[, .I[dummy_grk != 0]]
  notCodes <- dt[idx]$GEO

  logCmd <- is_log_write(value = notCodes, x = "codeShort", ...)
  is_verbose(length(idx), "Number of GEO codes need to be checked:", type = "warn2")
  is_check_geo(notCodes, control = control)
  is_verbose(msg = "99 or 9999 are added to the end of the code respectively")
  is_verbose(x = logCmd, msg = "To see these codes, run command:")

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
is_check_geo <- function(codes, control = FALSE){
  ## codes - Codes to display
  codes <- data.table::copy(codes)
  codesNot <- is_short_code(codes, n1 = 10, n2 = 6)
  ## is_verbose(msg = is_line_short(), type = "other")
  is_verbose(codesNot, "The codes:", type = "note")
  invisible(codes)
}


## Grunnkrets codes for change starts from 2002. All others before that need to
## have dummy from municipality to be able to recode to current geo code
is_problem_geo_before_2002 <- function(dt, dcode, type, year, con){
  # dcode - Problem codes
  # type - grunnkrets or bydel

  GEO <- Geo_Dummy <- oldCode <- currentCode <- i.newGEO <- newGEO <- NULL

  is_debug(deep = TRUE)
  yr <- unique(dt$AAR)

  yrOld <- any(yr < 2003)

  if (yrOld){
    geoTable <- paste0("kommune", year)
    # Kommune codes before 2003 only
    geoDT <- find_spec("geo-recode-dummy.sql", char = geoTable, con = con, char2 = 2003)
    data.table::setDT(geoDT)

    for (j in seq_len(ncol(geoDT))){
      data.table::set(geoDT, j = j, value = as.integer(geoDT[[j]]))
    }

    deleteLast <- switch(type,
                         bydel = "\\d{2}$",
                         grunnkrets = "\\d{4}$")

    dcode <- unique(as.integer(sub(deleteLast, "", dcode)))
    geoDT <- geoDT[oldCode %in% dcode]
    add99 <- switch(type,
                    bydel = "99",
                    grunnkrets = "9999")

    geoDT[, "newGEO" := paste0(currentCode, add99)]

    dt[ , Geo_Dummy := as.integer(sub(deleteLast, "", GEO)) ]
    dt[geoDT, on = c(Geo_Dummy = "oldCode"), "newGEO" := as.integer(i.newGEO)]
    dt[!is.na(newGEO), "GEO" := newGEO]
    dt[, c("Geo_Dummy", "newGEO") := NULL]
  }
  return(dt)
}

## Codes that can't be merged since it's not found in geo codebook database
is_problem_geo_merge <- function(x, y, vector = FALSE, control = FALSE, mode = c("recode", "delete"), ...){
  ## x - dataset
  ## y - geocodes
  ## vector - Either a data.frame or vector
  ## mode - to recode or to delete
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
    dcode <- is_problem_message(mode = mode, codes = dcode, control = control, ...)
  }

  return(dcode)
}

is_problem_message <- function(mode, codes, control = FALSE, ...){
  # mode - Either recode or delete

  scode <- is_short_code(codes, n1 = 10, n2 = 6)

  if (mode == "recode"){
    logCmd <- is_log_write(value = codes, x = "code99", ...)
    is_verbose(x = length(codes), msg = "Number of codes that fail to recode:", type = "warn2")
    is_verbose(x = scode, msg = "The codes:")
    is_verbose(x = "xxxx9999", msg = "They are now recoded with:", type = "note")
    is_verbose(x = logCmd, msg = "To see these codes, run command:")
  }

  if (mode == "delete"){
    logCmd <- is_log_write(value = codes, x = "codeDelete", ...)
    is_verbose(x = length(codes), msg = "Number of geo codes fail to recode and are excluded:", type = "warn2")
    is_verbose(x = scode, msg = "The codes:")
    is_verbose(x = logCmd, msg = "To see these codes, run command:")
  }

  invisible(codes)
}

## Grunnkrets that aren't able to be merged will be checked against municipality
## codes with unkown grunnkrets ie. xxxx9999
is_problem_geo <- function(dt, codes, type){
  # codes - the problem codes from is_problem_geo_merge()
  # type - type of granularity levels

  to99 <- switch(type,
                 bydel = "99",
                 grunnkrets = "9999")

  GEO <- NULL
  idx <- dt[, .I[GEO %in% codes]]
  dt <- is_replace_geo(dt, idx = idx,
                       from = "\\d{4}$",
                       to = to99)

  return(dt)
}



## Replace the code of selected index
is_replace_geo <- function(dt, idx, from, to = "9999"){
  # idx - Index row
  # from - A regular expression of what to change
  # to - what to replace with
  GEO <- NULL
  for (i in idx){
    code <- sub(from, "", dt[i]$GEO)
    grc <- paste0(code, to)
    dt[i, GEO := as.integer(grc)]
  }
  return(dt)
}


is_short_code <- function(x, n1 = 10, n2 = 6){
  ## n1 - maximum length before making cutoff
  ## n2 - maximum codes to display
  if (length(x) > n1){
    y <- sample(x, n2)
    codes <- c(y, "...")
  } else {
    codes <- x
  }

  paste_cols(codes)
}

