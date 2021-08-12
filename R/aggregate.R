#' @title Aggregate Data
#' @description Aggregate data according to the specification in `tbl_Filgruppe`.
#' @param dt Input data created with `read_org()` function
#' @param source What geographical granularity code that is available in the source data.
#'    This will be used for merging with the output from `do_norgeo()`
#' @param level Geographical granularity for aggregating data.
#' @param year Which year the georaphical code is valid for. If not specified, then
#'   it will be base on the year in source data ie. column `AAR`
#' @examples
#' \dontrun{
#'   # To aggregate source data with enumeration area codes ie. grunnkrets, to
#'   # manucipaltiy ie. kommune
#'   dt <- read_org("BEFOLKNING")
#'   DT <- do_aggregate(dt, source = "grunnkrets", level = "kommune")
#' }
#' @import data.table
#' @export
do_aggregate <- function(dt = NULL,
                         source = c("grunnkrets",
                                    "fylke",
                                    "kommune",
                                    "bydel"),
                         level = c("grunnkrets",
                                   "fylke",
                                   "kommune",
                                   "bydel"),
                         year = NULL){
  VAL <- GEO <- NULL
  is_null(dt)
  dtt <- data.table::copy(dt)

  source <- tolower(source)
  level <- tolower(level)
  source <- match.arg(source)
  level <- match.arg(level)

  aggCols <- c(level, names(dt)[!names(dtt) %in% c("GEO", "VAL") ])

  geoFile <- is_path_db(getOption("orgdata.geo"), check = TRUE)
  geo <- KHelse$new(geoFile)

  ## validTo in the database is a character
  if (is.null(year)){
    yr <- dtt$AAR[1]
  } else {
    yr <- as.character(year)
  }

  geoDT <- find_spec("geo-code.sql", con = geo$dbconn, char = source, num = yr, opposite = TRUE)
  data.table::setDT(geoDT)

  intCols <- c("code", "grunnkrets", "kommune", "fylke", "bydel")
  geoDT[, (intCols) := lapply(.SD, as.integer), .SDcols = intCols]
  deleteVar <- c("code", "level", "name", "validTo")
  keepVar <- setdiff(names(geoDT), deleteVar)

  ## TODO read_file should convert integer variables
  if(class(dtt$GEO) == "character"){
    dtt[, GEO := as.integer(GEO)]
  }

  dtt[geoDT, on = c(GEO = "code"), (keepVar) := mget(keepVar)]

  xCols <- is_set_list(level = level,
                       srcCols = aggCols)

  ## DT <- data.table::cube(dtt, j = c(VAL = sum(VAL, na.rm = TRUE)), by = aggCols)
  DT <- data.table::groupingsets(
                      dtt,
                      j = list(VAL = sum(VAL, na.rm = TRUE)),
                      by = aggCols,
                      sets = xCols
                    )

  on.exit(rm(dtt, geoDT))
  gc()
  data.table::setnames(DT, level, "GEO")
}

#' @title Get Aggregate Specification
#' @description
#' Get the specification on how the data will be aggregated to
#' different geographical levels ie. county, manucipality, town etc.
#' @inheritParams read_org
#' @inheritParams get_split
#' @inheritParams find_column_input
#' @export
get_aggregate <- function(group = NULL, con = NULL, spec = NULL) {
  is_null_also(group, spec)
  is_null_both(group, spec)

  if (is.null(spec)) {
    spec <- find_spec("filegroups.sql", group, con)
  }
  input <- find_column_input(spec, "AGGREGERE")
  is_separate(input, sep = ",")
}


## Helper ----------------------------------------
is_set_list <- function(level, srcCols){
  # level - Geo granularity to aggregate.R
  # srcCols - Colnames of source data to be aggregated
  cols <- is.element(c("KJONN","ALDER"), srcCols)

  vars <- c("KJONN", "ALDER")
  vars2 <- c(level, "AAR")
  var01 <- c(vars2, vars[1])
  var02 <- c(vars2, vars[2])
  var03 <- c(vars2, vars)

  if (sum(cols) == 2){
    list(var01, var02, var03, srcCols)
  } else {
    col <- which(cols == 1)
    list(c(vars2, vars[col]),
         srcCols)
  }
}
