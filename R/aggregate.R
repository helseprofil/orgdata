#' @title Aggregate Data
#' @description Aggregate data according to the specification in `tbl_Filgruppe`
#'   in `AGGREGERE` column. The input in argument `source` must be a lower
#'   granularity level than the `level` input.
#' @inheritParams do_split
#' @param source What geographical granularity codes that is available in the
#'   source data. This will be used for merging with the geo codebook generated
#'   from `geo_map()` ie. from `tblGeo` in geo database
#' @param level Geographical granularity for aggregating data. See `getOption("orgdata.geo.levels")`
#' @inheritParams do_geo_recode
#' @param aggregate.col Other columns to aggregate other than the standard ie.
#'   `UTDANN`, `LANDSSB`, `LANDBAK` and `INNVKAT`
#' @param geoDT Geo codes to aggregate dataset with
#' @param check If TRUE then output will keep variables for geographical levels
#'   without aggregating it. This is useful to check for geographical codes that
#'   are missing. Else use `options(orgdata.aggregate = FALSE)`
#' @inheritParams do_geo_recode
#' @param wide Column(s) from reshape wide
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
                         level = getOption("orgdata.geo.levels"),
                         year = NULL,
                         aggregate.col = NULL,
                         geoDT = NULL,
                         check = NULL,
                         base = NULL,
                         control = FALSE,
                         wide = NULL) {

  VAL1 <- GEO <- AAR <- NULL
  fylke <- kommune <- bydel <- LEVEL <- LANDSSB <- NULL

  is_debug()
  is_null(dt)
  dt <- data.table::copy(dt)

  if(is.null(check)) check <- getOption("orgdata.debug.aggregate")
  if(is.null(base)) base <- getOption("orgdata.recode.base")

  source <- tolower(source)
  level <- tolower(level)
  source <- match.arg(source)
  level <- match.arg(level)

  ## LANDSSB creates misunderstanding when aggregate due to the recode in
  ## LANDBAK and INNVKAT where 0 becomes 20 while keeping 0 as total in LANDBAK
  ## and INNVKAT
  if (any(names(dt) == "LANDSSB")) dt[, LANDSSB := NULL]

  msg <- paste0("Starts aggregating data from ", source, " to")
  is_verbose(x = level, msg = msg, ctrl = FALSE)

  colVals <- paste0("VAL", 1:getOption("orgdata.vals"))

  # Don't aggregate columns reshape wide
  if (!is.null(wide)){
    colVals <- c(colVals, wide)
  }

  aggNot <- c("GEO", colVals, "origin")
  aggYes <- setdiff(names(dt), aggNot)
  aggCols <- c(level, aggYes)

  deleteVar <- c("code", "level", "name", "validto")
  keepVar <- setdiff(names(geoDT), deleteVar)

  ## is_verbose("Merging geo codes...", type = "note")
  dt[geoDT, on = c(GEO = "code"), (keepVar) := mget(keepVar)]

  ## Breakpoint here to check the missing GEO when merging
  if (check) {
    is_debug_warn("`orgdata.debug.aggregate`")
    orgEnv$status <- 0
    return(dt)
  }

  if (level %in% c("kommune", "fylke")){
    dt <- is_level_na(dt = dt, level = level)
  } else {
    dt <- dt[!is.na(get(level))]
  }

  xCols <- is_set_list(
    level = level,
    srcCols = aggCols,
    colx = aggregate.col,
    dt
  )

  colj <- intersect(colVals, names(dt))

  DT <- data.table::groupingsets(
    dt,
    j = lapply(.SD, sum, na.rm = TRUE),
    by = aggCols,
    sets = xCols,
    .SDcols = colj
  )

  DT[, LEVEL := level]
  data.table::setnames(DT, level, "GEO")
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
  is_debug(deep = TRUE)
  is_null_both(group, spec)
  is_not_null_both(group, spec)

  if (is.null(spec)) {
    spec <- find_spec("filegroups.sql", group, con)
  }
  input <- find_column_multi(spec, "AGGREGERE")
  ## is_separate(input, sep = ",")
  level <- vector(mode = "list", length = length(input))
  level <- stats::setNames(level, input)
  for (i in input){
    level[[i]] <- is_geo_names(i)

  }

  if (isTRUE(getOption("orgdata.debug.geo")) | isTRUE(getOption("orgdata.debug.aggregate"))){
    level <- list("kommune")
  }

  unlist(level)
}


## Helper ----------------------------------------------------------------

is_geo_names <- function(x){
  abv <- unlist(getOption("orgdata.geo.abv"))
  names(abv)[abv == x]
}

## Handling missing geo levels
is_level_na <- function(dt, level){
  GEO <- kommune <- fylke <- bydel <- NULL
  switch(level,
         kommune = dt[is.na(kommune), kommune := as.integer(gsub("\\d{4}$", "", GEO))],
         fylke =  dt[is.na(fylke), fylke := as.integer(gsub("\\d{6}$", "", GEO))],
         bydel = {dt <- dt[!is.na(bydel)]},
         dt
         )
}

## Create list of combination to aggregate in groupingsets
is_set_list <- function(level, srcCols, colx = NULL, dt = NULL) {
  # level - Geo granularity to aggregate.R
  # srcCols - Colnames of source data to be aggregated
  # colx - Colnames to aggregate other than standard

  is_debug(deep = TRUE)

  ## Don't aggregate these columns
  tabs <- paste0("TAB", 1:getOption("orgdata.tabs"))
  aggNot <- c(level, "AAR", "KJONN", "ALDER", tabs)
  ## Find the columns that exist in the dataset
  vars <- intersect(aggNot, srcCols)

  if (!is.null(colx)){
    aggNot <- setdiff(aggNot, colx)
    vars <- intersect(aggNot, srcCols)
  }

  sameVars <- identical(vars, srcCols)

  if (sameVars){
    is_validate_NA(vars, dt)
    listSet <- list(vars)
  } else {
    aggCols <- setdiff(srcCols, vars)
    is_validate_NA(aggCols, dt)
    listSet <- unlist(lapply(seq_along(aggCols),
                             utils::combn,
                             x = aggCols,
                             simplify = FALSE),
                      recursive = FALSE)

    for (i in seq_along(listSet)){
      listSet[[i]] <- c(vars, listSet[[i]])
    }

    setNr <- length(listSet)
    listSet[[setNr + 1]] <- vars
  }

  return(listSet)
}


## Check column to be aggregated doesn't have NA
## since aggregating produce NA to represent total
is_validate_NA <- function(cols, dt){

  for (i in cols){
    val <- dt[is.na(get(i)), .N]

    if (val > 0){
      msg01 <- "NA value is not allowed when aggregating selected column(s)!"
      msg02 <- "This column has NA and must be recoded:"
      msgAgg <- paste0(msg01, "\n", msg02)
      is_stop(msg = msgAgg, var = paste_cols(i))
    }
  }
}

## Download geo dataset
is_geo_cast <- function(source, year){
# source - Geo code granularity in the dataset ie. to be aggreagated from
  is_debug(deep = TRUE)
  geoFile <- is_path_db(getOption("orgdata.geo"), check = TRUE)
  geoDB <- is_conn_db(geoFile)

  ## Cast geo levels ie. aggregate to different geo levels
  geoDT <- find_spec(
    "geo-code.sql",
    con = geoDB$dbconn,
    char = source,
    char2 = year,
    opposite = TRUE
  )
  data.table::setDT(geoDT)

  colsRename <- tolower(names(geoDT))
  data.table::setnames(geoDT, names(geoDT), colsRename)

  ## Columns that are type integer
  intCols <- c("code", getOption("orgdata.geo.levels"))
  intCols <- intersect(names(geoDT), intCols)
  geoDT[, (intCols) := lapply(.SD, as.integer), .SDcols = intCols]
  geoDT[, "land" := 0L]
  ncols <- names(geoDT)!="batch"
  data.table::setcolorder(geoDT, c(names(geoDT)[ncols], "batch"))
}
