#' @title Aggregate Data
#' @description Aggregate data according to the specification in `tbl_Filgruppe`.
#' @inheritParams do_split
#' @param source What geographical granularity code that is available in the source data.
#'    This will be used for merging with the output from `do_norgeo()`
#' @param level Geographical granularity for aggregating data.
#' @param year Which year the georaphical code is valid for. If not specified, then
#'   it will be base on the year in source data ie. column `AAR`
#' @param check If TRUE then output will not be aggregated. This is useful to check
#'   for geographical codes that are missing. Else use `options(orgdata.aggregate = FALSE)`
#' @examples
#' \dontrun{
#' # To aggregate source data with enumeration area codes ie. grunnkrets, to
#' # manucipaltiy ie. kommune
#' dt <- make_file("BEFOLKNING")
#' DT <- do_aggregate(dt, source = "grunnkrets", level = "kommune")
#' }
#' @import data.table
#' @family aggregate functions
#' @export
do_aggregate <- function(dt = NULL,
                         source = c(
                           "grunnkrets",
                           "fylke",
                           "kommune",
                           "bydel"
                         ),
                         level = c(
                           "grunnkrets",
                           "fylke",
                           "kommune",
                           "bydel"
                         ),
                         year = NULL,
                         check = FALSE) {

  VAL1 <- GEO <- AAR <- fylke <- kommune <- LEVEL <- NULL

  is_debug()
  is_null(dt)
  dt <- data.table::copy(dt)

  source <- tolower(source)
  level <- tolower(level)
  source <- match.arg(source)
  level <- match.arg(level)

  aggNot <- c("GEO", "VAL1")
  aggYes <- setdiff(names(dt), aggNot)
  aggCols <- c(level, aggYes)

  geoFile <- is_path_db(getOption("orgdata.geo"), check = TRUE)
  geoDB <- KHelse$new(geoFile)

  ## validTo in the database is a character
  if (!is.null(year)) {
    yr <- dt[AAR == year, ][1]
  } else {
    yr <- as.integer(format(Sys.Date(), "%Y"))
  }

  ## recode GEO
  code <- get_geo_recode(con = geoDB$dbconn, type = source, year = yr)
  dt <- do_geo_recode(dt = dt, code = code)

  ## geoDT <- find_spec("geo-code-all.sql", value = source, con = geo$dbconn)
  geoDT <- find_spec(
    "geo-code.sql",
    con = geoDB$dbconn,
    char = source,
    num = yr,
    opposite = TRUE
  )
  data.table::setDT(geoDT)

  ## Columns that are type integer
  intCols <- c("code", "grunnkrets", "kommune", "fylke", "bydel")
  geoDT[, (intCols) := lapply(.SD, as.integer), .SDcols = intCols]

  deleteVar <- c("code", "level", "name", "validTo")
  keepVar <- setdiff(names(geoDT), deleteVar)

  ## TODO read_file should convert integer variables
  if (class(dt$GEO) == "character") {
    dt[, GEO := as.integer(GEO)]
  }

  ## is_verbose("Merging geo codes...", type = "message")
  dt[geoDT, on = c(GEO = "code"), (keepVar) := mget(keepVar)]

  ## Breakpoint here to check the missing GEO when merging
  if (check) {
    warning("Aggregating data isn't completed!")
    return(dt)
  }

  dt[is.na(kommune), kommune := as.integer(gsub("\\d{4}$", "", GEO))]
  dt[is.na(fylke), fylke := as.integer(gsub("\\d{6}$", "", GEO))]

  xCols <- is_set_list(
    level = level,
    srcCols = aggCols
  )

  DT <- data.table::groupingsets(
    dt,
    j = list(VAL1 = sum(VAL1, na.rm = TRUE)),
    by = aggCols,
    sets = xCols
  )
  DT[, LEVEL := level]
  data.table::setnames(DT, level, "GEO")
}

#' @title Recode Aggregated Variables
#' @description Recode aggregated variables to represent all values either as `0`
#'  or `10` integer variables and `Tot` for string variables. Value `10` representing
#'  total is only used for `LANDB` since it already has `0` as one of it's existing
#'  value.
#' @inheritParams do_split
#' @family aggregate functions
#' @export
do_aggregate_recode <- function(dt) {
  is_debug()
  cols <- is_aggregate_standard_cols()

  dt <- is_aggregate_recode(dt, cols$intMin, to = 0)
  dt <- is_aggregate_recode(dt, cols$intMax, to = 10)
  dt <- is_aggregate_recode(dt, cols$chrCols, to = "Tot")

  invisible(dt)
}


#' @title Get Aggregate Specification
#' @description
#' Get the specification on how the data will be aggregated to
#' different geographical levels ie. county, manucipality, town etc.
#' @inheritParams make_file
#' @inheritParams get_split
#' @inheritParams find_column_input
#' @family aggregate functions
#' @export
get_aggregate <- function(group = NULL, con = NULL, spec = NULL) {
  is_null_both(group, spec)
  is_not_null_both(group, spec)

  if (is.null(spec)) {
    spec <- find_spec("filegroups.sql", group, con)
  }
  input <- find_column_input(spec, "AGGREGERE")
  is_separate(input, sep = ",")
}


## Helper ----------------------------------------

is_aggregate_standard_cols <- function(){
  ## Total is 0
  intMin <- c("UTDANN", "SIVILSTAND", "LANDF")
  ## Total is 10
  intMax <- "LANDB"
  ## Total is Tot
  chrCols <- "LANDSSB"

  list(intMin = intMin, intMax = intMax, chrCols = chrCols)
}

is_aggregate_recode <- function(dt, cols, to){
  isCols <- sum(is.element(cols, names(dt))) > 0

  if (isCols){
    for (j in seq_along(cols)){
      col <- cols[j]
      data.table::set(dt, i = which(is.na(dt[[col]])), j = col, value = to)
    }
  }
  invisible(dt)
}

## Create list to aggregate in groupingsets
is_set_list <- function(level, srcCols) {
  # level - Geo granularity to aggregate.R
  # srcCols - Colnames of source data to be aggregated
  cols <- is.element(c("KJONN", "ALDER"), srcCols)

  vars <- c("KJONN", "ALDER")
  vars2 <- c(level, "AAR")
  vars3 <- c(vars2, vars)

  if (sum(cols) == 2) {
    list(vars3, srcCols)
  } else {
    list(vars2, srcCols)
  }
}

is_match_arg <- function(arg){
  arg <- tolower(arg)
  arg <- match.arg(arg)
}


#' @title Use Original Columnames
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is either to use original columnames or use the standard columnames.
#' It's deprecated because output MUST use standard columnames.
#' @keywords internal
is_active <- function(active = TRUE, .env = parent.frame()){

  lifecycle::deprecate_stop("0.0.9", "is_active()")

  VAL1 <- NULL
  if (isFALSE(active)){
    DT <- data.table::cube(.env$dt, j = c(VAL1 = sum(VAL1, na.rm = TRUE)), by = .env$aggCols)
    data.table::setnames(DT, c("grunnkrets", "V1"), c(.env$geo, .env$val))
  } else {
    DT <- data.table::groupingsets(
      .env$dt,
      j = list(VAL1 = sum(VAL1, na.rm = TRUE)),
      by = .env$aggCols,
      sets = .env$xCols
    )
    data.table::setnames(DT, .env$level, "GEO")
  }
  invisible(DT)
}
