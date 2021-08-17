#' @title Aggregate Data
#' @description Aggregate data according to the specification in `tbl_Filgruppe`.
#' @inheritParams do_split
#' @param source What geographical granularity code that is available in the source data.
#'    This will be used for merging with the output from `do_norgeo()`
#' @param level Geographical granularity for aggregating data.
#' @param year Which year the georaphical code is valid for. If not specified, then
#'   it will be base on the year in source data ie. column `AAR`
#' @param check If TRUE then output will not be aggregated. This is useful to check
#'   for geographical codes that are missing.
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
                         year = NULL,
                         check = FALSE){
  VAL <- GEO <- fylke <- kommune <- NULL
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

  ## geoDT <- find_spec("geo-code-all.sql", value = source, con = geo$dbconn)
  geoDT <- find_spec("geo-code.sql", con = geo$dbconn, char = source, num = yr, opposite = TRUE)
  data.table::setDT(geoDT)

  intCols <- c("code", "grunnkrets", "kommune", "fylke", "bydel")

  ## for (j in seq_len(length(intCols))){
  ##   if(class(geoDT[[j]])== "character")
  ##     data.table::set(geoDT, j = j, value = as.integer(geoDT[[j]]))
  ## }
  geoDT[, (intCols) := lapply(.SD, as.integer), .SDcols = intCols]

  deleteVar <- c("code", "level", "name", "validTo")
  keepVar <- setdiff(names(geoDT), deleteVar)

  ## TODO read_file should convert integer variables
  if(class(dtt$GEO) == "character"){
    dtt[, GEO := as.integer(GEO)]
  }

  dtt[geoDT, on = c(GEO = "code"), (keepVar) := mget(keepVar)]

  ## Breakpoint here to check the missing GEO when merging
  if (check){
    warning("Aggregating data isn't completed!")
    return(dtt)
  }

  dtt[is.na(kommune), kommune := as.integer(gsub("\\d{4}$", "", GEO))]
  dtt[is.na(fylke), fylke := as.integer(gsub("\\d{6}$", "", GEO))]

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

#' @title Recode Aggregated Variables
#' @description Recode aggregated variables to represent all values either as `0`
#'  integer variables and `Tot` for string variables.
#' @inheritParams do_split
#' @export
do_aggregate_recode <- function(dt){
  ## Total is 0
  intMin <- c("UTDANN", "SIVILSTAND", "LANDF")
  ## Total is 10
  intMax <- "LANDB"
  ## Total is Tot
  chrCols <- "LANDBAK"

  for (j in seq_len(length(intMin))){
    col <- intMin[j]
    data.table::set(dt, i = which(is.na(dt[[col]])), j = col, value = 0)
  }
  dt[is.na(get(intMax)), (intMax) := 10]
  dt[is.na(get(chrCols)), (chrCols) := "Tot"]
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
  is_null_both(group, spec)
  is_not_null_both(group, spec)

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
  ## var01 <- c(vars2, vars[1])
  ## var02 <- c(vars2, vars[2])
  var03 <- c(vars2, vars)

  if (sum(cols) == 2){
    ## list(var01, var02, var03, srcCols)
    list(var03, srcCols)
  } else {
    ## col <- which(cols == 1)
    ## list(c(vars2, vars[col]),
    ##      srcCols)
    list(vars2, srcCols)
  }
}
