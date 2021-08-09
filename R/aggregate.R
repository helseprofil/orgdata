#' @title Aggregate Data
#' @description Aggregate data according to the specification in `tbl_Filgruppe`
#' @inheritParams do_split
#' @param source Geo code available in the source data for merging
#' @param level Geographical levels for aggregating data
#' @import data.table
#' @export
do_aggregere <- function(dt = NULL,
                         source = c("G", "F", "K", "B"),
                         level = c("G", "F", "K", "B")){
  is_null(dt)

  source <- match.arg(source)
  level <- match.arg(level)

  src <- is_geo_level(source)
  aggr <- is_geo_level(level)

  aggCols <- c(aggr, names(dt)[!names(dt) %in% c("GEO", "VAL") ])

  geoFile <- is_path_db(getOption("orgdata.geo"), check = TRUE)
  geo <- KHelse$new(geoFile)
  geoDT <- find_spec("geo-code.sql", src, geo$dbconn)
  data.table::setDT(geoDT)

  intCols <- c("code", "grunnkrets", "kommune", "fylke", "bydel")
  geoDT[, (intCols) := lapply(.SD, as.integer), .SDcols = intCols]
  deleteVar <- c("code", "level", "name", "validTo")
  keepVar <- setdiff(names(geoDT), deleteVar)

  dt[geoDT, on = c(GEO = "code"), (keepVar) := mget(keepVar)]

  xCols <- c(aggr, "AAR", "KJONN", "ALDER")
  ## aggCols <- (names(dt))[!names(dt) %in% c("name", "validTo")]
  DT <- data.table::groupingsets(
    dt,
    j = .(VAL = sum(VAL, na.rm = TRUE)),
    by = aggCols,
    sets = list(
      aggCols,
      xCols
    )
  )

  data.table::setnames(DT, aggr, "GEO")
}

#' @title Get Aggregate Specification
#' @description
#' Get the specification on how the data will be aggregated to
#' different geographical levels ie. county, manucipality, town etc.
#' @inheritParams read_org
#' @inheritParams get_split
#' @inheritParams find_column_input
#' @export
get_aggregere <- function(group = NULL, con = NULL, spec = NULL) {
  is_null_also(group, spec)
  is_null_both(group, spec)

  if (is.null(spec)) {
    spec <- find_spec("filegroups.sql", group, con)
  }
  input <- find_column_input(spec, "AGGREGERE")
  is_separate(input, sep = ",")
}


## Helper ----------------------------------------
is_geo_level <- function(x){
  aggr <- switch(x,
                 G = "grunnkrets",
                 F = "fylke",
                 K = "kommune",
                 B = "bydel")
}
