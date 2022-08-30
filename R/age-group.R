#' Create age categories
#' @description
#' Create age categories either by interval or specified age categories.
#' @param dt Dataset
#' @param interval Age interval
#' @examples
#' \dontrun{
#' dd
#' }
#' @export
age_category <- function(dt = NULL, interval = NULL) {
  UseMethod("age_category", interval)
}

#' @method age_category default
#' @export
age_category.default <- function(dt, interval) {
  message("Selected age category: ", interval)
  stop(sprintf("Age categories not valid: `%s`", interval))
}

#' @method age_category val
#' @export
age_category.val <- function(dt, interval){

  ALDER <- ageid <- grp <- ageGRP <- alderGRP <- NULL
  up <- lo <- NULL

  vals <- paste0("VAL", 1:getOption("orgdata.vals"))
  gpv <- setdiff(names(dt), vals)

  ## Always min = 0
  mix <- dt[, list(min = 0,
                   max = max(ALDER, na.rm = TRUE))]
  ageBrk <- c(seq(mix[["min"]], mix[["max"]], by = interval), Inf)

  dt[, grp := cut(ALDER, breaks = ageBrk, right = FALSE), by = mget(gpv)][, grp := as.character(grp)]
  dt[, ageid := .GRP, by = c(gpv, "grp")]

  vals <- grep("^VAL", names(dt), value = TRUE)
  for (i in vals){
    vai <- tolower(i)
    dt[, (vai) := sum(get(i), na.rm = TRUE), by = ageid]
    dt[, (i) := get(vai)]
    dt[, (vai) := NULL]
  }

  dt <- dt[, .SD[1], by = ageid]
  dt[, ageGRP := sub("\\[(.*)\\)", "\\1", grp)]

  ageVars <- c("lo", "up")
  dt[, (ageVars) := data.table::tstrsplit(ageGRP, ",")]

  for (j in ageVars){
    suppressWarnings(data.table::set(dt, j = j, value = as.numeric(dt[[j]])))
  }

  dt[, up := up - 1]
  dt[up != Inf, alderGRP := paste0(lo, "_", up)]
  dt[up == Inf, alderGRP := paste0(lo, "+")]
  dt[, ALDER := alderGRP]

  delVals <- c("grp", "ageGRP", "alderGRP", ageVars)
  dt[, (delVals) := NULL]
  return(dt)
}



## Helper ----------
