#' @title Aggregate Data
#' @description Aggregate data according to the specification in `tbl_Filgruppe`
#'   in `AGGREGERE` column. The input in argument `source` must be a lower
#'   granularity level than the `level` input.
#' @inheritParams do_split
#' @param source What geographical granularity codes that is available in the
#'   source data. This will be used for merging with the output from
#'   `geo_level()`
#' @param level Geographical granularity for aggregating data to.
#' @param year Which year the georaphical code is valid for. If not specified,
#'   then it will be base on the year in source data ie. column `AAR`
#' @param aggregate.col Other columns to aggregate other than the standard ie.
#'   `UTDANN`, `LANDSSB`, `LANDBAK` and `INNVKAT`
#' @param check If TRUE then output will not be aggregated. This is useful to
#'   check for geographical codes that are missing. Else use
#'   `options(orgdata.aggregate = FALSE)`
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
                         aggregate.col = NULL,
                         check = getOption("orgdata.debug.aggregate")) {

  VAL1 <- GEO <- AAR <- NULL
  fylke <- kommune <- bydel <- LEVEL <- LANDSSB <- NULL

  is_debug()
  is_null(dt)
  dt <- data.table::copy(dt)

  source <- tolower(source)
  level <- tolower(level)
  source <- match.arg(source)
  level <- match.arg(level)

  ## LANDSSB creates misunderstanding when aggregate due to the recode in
  ## LANDBAK and INNVKAT where 0 becomes 20 while keeping 0 as total in LANDBAK
  ## and INNVKAT
  if (any(names(dt) == "LANDSSB")) dt[, LANDSSB := NULL]

  msg <- paste0("Starts aggregating data from ", source, " to")
  is_verbose(x = level, msg = msg)

  colVals <- paste0("VAL", 1:getOption("orgdata.vals"))
  aggNot <- c("GEO", colVals)
  aggYes <- setdiff(names(dt), aggNot)
  aggCols <- c(level, aggYes)

  geoFile <- is_path_db(getOption("orgdata.geo"), check = TRUE)
  geoDB <- KHelse$new(geoFile)

  ## validTo in the database `tblGeo` is a character
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
    char2 = yr,
    opposite = TRUE
  )
  data.table::setDT(geoDT)

  ## Columns that are type integer
  intCols <- c("code", "grunnkrets", "kommune", "fylke", "bydel")
  geoDT[, (intCols) := lapply(.SD, as.integer), .SDcols = intCols]

  deleteVar <- c("code", "level", "name", "validTo")
  keepVar <- setdiff(names(geoDT), deleteVar)

  ## TODO read_file should convert integer variables
  if (is(dt$GEO, "character")) {
    dt[, GEO := as.integer(GEO)]
  }

  ## is_verbose("Merging geo codes...", type = "note")
  dt[geoDT, on = c(GEO = "code"), (keepVar) := mget(keepVar)]

  ## Breakpoint here to check the missing GEO when merging
  if (check) {
    warning("Aggregating data isn't completed!")
    return(dt)
  }

  dt <- is_level_na(dt = dt, level = level)

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
  is_null_both(group, spec)
  is_not_null_both(group, spec)

  if (is.null(spec)) {
    spec <- find_spec("filegroups.sql", group, con)
  }
  input <- find_column_multi(spec, "AGGREGERE")
  ## is_separate(input, sep = ",")
  level <- vector(mode = "list", length = length(input))
  for (i in seq_along(input)){
    gg <- switch(input[i],
                 "F" = "fylke",
                 "K" = "kommune",
                 "B" = "bydel",
                 "grunnkrets"
                 )
    level[[i]] <- gg
  }
  unlist(level)
}


## Helper ----------------------------------------------------------------

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
      is_stop("This column has NA and need to recode:", var = paste_cols(i))
    }
  }
}
