#' Define age categories
#' @description Define and create age categories either by age interval or
#'   specified age categories. How to define the age categories in `EXTRA`
#'   column in Access is shown in the example below. This function is only
#'   applicable in the filegroup table. The input dataset must be in
#'   `data.table` format if using the function outside Access. Convert dataset
#'   to `data.table` with `data.table::setDT(DF)`.
#' @description Age categories can be specified as follows:
#'   - Specific interval
#'   eg. every 5 years. Interval with odd numbers will use minimum age of 0 and
#'   maximum age is 85+, while even number uses maximum age of 80+ eg `AgeCat(5)`.
#'   - Varied
#'   intervals where lower bound need to be specified eg. `0, 19, 45, 65, 80`
#'   for age categories of 0-18,19-44,45-64,65-79, 80+ eg. `AgeCat(0, 19, 45, 65, 80)`.
#'   - Mix between specific and varied intervals with `[x]` eg.
#'   `0, 5, 10, 20, 30, 40, 55+`. Here the first 3 groups has interval of 5
#'   and followed by interval of 10 eg. `AgeCat(0, 5, 10, [10], 40, 55)`.
#' @param dt Dataset
#' @param interval Age interval
#' @example inst/codes/example-age-category.R
#' @family extra arguments
#' @export

find_age_category <- function(dt = NULL, interval = NULL) {
  UseMethod("find_age_category", object = interval)
}

#' @method find_age_category default
#' @export
find_age_category.default <- function(dt, interval) {
  is_color_txt(interval, "Selected age category:")
  is_stop("Age categories not valid:", interval)
}

# Interval value to categorize age. The minimum age will always be
# 0 while maximum age is 80 for even age interval and 85 for odd age
# interval.
#' @method find_age_category val
#' @export
find_age_category.val <- function(dt, interval){
  is_debug()
  is_color_txt(x = interval, msg = "Creating age category with year-interval of", emoji = TRUE)

  ## Age lower and upper limit for odd and even number
  valOdd <- interval %% 2
  maxAge <- ifelse(valOdd == 0, 80, 85)
  minAge <- 0

  ageBrk <- c(seq(from = minAge, to = maxAge, by = interval), Inf)
  dt <- is_recode_age(dt, category = ageBrk)
  return(dt)
}

#' @method find_age_category cat
#' @export
find_age_category.cat <- function(dt, interval){
  is_debug()
  txt <- paste(interval, collapse = ", ")
  is_color_txt(x = paste0(txt, "+"), msg = "Creating age category", emoji = TRUE)
  ageBrk <- c(interval, Inf)
  dt <- is_recode_age(dt, category = ageBrk)
  return(dt)
}

#' @method find_age_category mix
#' @export
find_age_category.mix <- find_age_category.cat

## Helper ----------

is_recode_age <- function(dt, category){
  is_debug(deep = TRUE)
  to <- NULL
  vals <- paste0("VAL", 1:getOption("orgdata.vals"))

  ageVec <- sort( unique(dt[["ALDER"]]) )
  dtCode <- is_age_codebook(x = ageVec, category = category)
  dt <- dt[dtCode, on = "ALDER"]
  dt[, "ALDER" := to][, "to" := NULL]

  ## ID to group rows by vars other than VAR
  idCol <- "ageid"
  gpv <- setdiff(names(dt), vals)
  data.table::setkeyv(dt, gpv)
  dt[, (idCol) := .GRP, by = mget(gpv)]

  data.table::setkeyv(dt, idCol)

  vals <- grep("^VAL", names(dt), value = TRUE)
  for (i in vals){
    vai <- tolower(i)
    dt[, (vai) := sum(get(i), na.rm = TRUE), by = get(idCol)]
    dt[, (i) := get(vai)]
      dt[, (vai) := NULL]
    }

    dt <- dt[, .SD[1], by = idCol]
    dt[, (idCol) := NULL]
    return(dt)
  }

# Create codeboook to recode age
is_age_codebook <- function(x, category){
  # x - Numeric vector eg. age
  is_debug(deep = TRUE)
  ALDER <- grp <- ageGRP <- up <- lo <- NULL

  dt <- data.table::data.table(ALDER = x, grp = NA)
  dt[, "grp" := cut(ALDER, breaks = category, right = FALSE)]
  dt[, "ageGRP" := sub("\\[(.*)\\)", "\\1", grp)]

  ageVars <- c("lo", "up")
  dt[, (ageVars) := data.table::tstrsplit(ageGRP, ",")]

  for (j in ageVars){
    suppressWarnings(data.table::set(dt, j = j, value = as.numeric(dt[[j]])))
  }

  dt[, "up" := up - 1]
  agp <- "alderGRP"
  dt[up != Inf, (agp) := paste0(lo, "_", up)]
  dt[up == Inf, (agp) := paste0(lo, "+")]

  delCols <- c(ageVars, "ageGRP", "grp")
  dt[, (delCols) := NULL]
  data.table::setnames(dt, agp, "to")
  return(dt)
}
