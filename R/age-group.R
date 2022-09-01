#' Create age categories
#' @description Create age categories either by age interval or specified age
#'   categories. How to define the age categories in `EXTRA` column in Access is
#'   shown in the example below.
#' @description Age categories can be specified as follows:
#'   - Specific interval eg. every 5 years. Interval with odd numbers will use minimum age of 0 and maximum age is 85+, while even number uses maximum age of 80+.
#'   - Specified interval eg. 0-18, 19-44, 45-64, 65-79, 80+
#' @param dt Dataset
#' @param interval Age interval
#' @examples
#' \dontrun{
#' AgeCat(5) #Group age for every 5 years with min 0 and max 85+
#' AgeCat(10) #Group age for every 10 years with min 0 and max 80+
#' AgeCat(0, 19, 45, 65, 80) #Age group of 0-18, 19-44, 45-64, 65-79, 80+
#' }
#' @family extra arguments
#' @export
find_age_category <- function(dt = NULL, interval = NULL) {
  UseMethod("find_age_category", interval)
}

#' @method find_age_category default
#' @export
find_age_category.default <- function(dt, interval) {
  message("Selected age category: ", interval)
  stop(sprintf("Age categories not valid: `%s`", interval))
}

# Interval value to categorize age. The minimum age will always be
# 0 while maximum age is 80 for even age interval and 85 for odd age
# interval.
#' @method find_age_category val
#' @export
find_age_category.val <- function(dt, interval){
  is_color_txt(x = interval, msg = "Creating age category with year-interval of", emoji = TRUE)

  ## Age lower and upper limit for odd and even number
  valOdd <- interval %% 2
  maxAge <- ifelse(valOdd == 0, 80, 85)
  minAge <- 0

  ageBrk <- c(seq(from = minAge, to = maxAge, by = interval), Inf)
  dt <- make_age_cat(dt, category = ageBrk)
  return(dt)
}

#' @method find_age_category cat
#' @export
find_age_category.cat <- function(dt, interval){

  txt <- paste(interval, collapse = ", ")
  is_color_txt(x = paste0(txt, "+"), msg = "Creating age category", emoji = TRUE)
  ageBrk <- c(interval, Inf)
  dt <- make_age_cat(dt, category = ageBrk)
  return(dt)
}


## Helper ----------

make_age_cat <- function(dt, category){

  ALDER <- ageid <- ageGRP <- alderGRP <- NULL
  grp <- up <- lo <- grpid <- NULL

  vals <- paste0("VAL", 1:getOption("orgdata.vals"))
  gpv <- setdiff(names(dt), vals)

  dt[, grpid := .GRP, by = mget(gpv)]
  dt[, grp := cut(ALDER, breaks = category, right = FALSE), by = grpid][, grp := as.character(grp)]

  idVars <- c("grpid", "grp")
  dt[, ageid := .GRP, by = mget(idVars)]

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

  delVals <- c("ageGRP", "alderGRP", idVars, ageVars)
  dt[, (delVals) := NULL]
  return(dt)
}
