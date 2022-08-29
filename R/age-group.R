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

age_category.val <- function(dt, interval){

  ALDER <- ageid <- grp <- NULL
  vals <- paste0("VAL", 1:getOption("orgdata.vals"))
  gpv <- setdiff(names(dt), vals)
  mix <- dt[, .(min = min(ALDER, na.rm = TRUE),
                max = max(ALDER, na.rm = TRUE) + 2)] #+2 ensure inclusion of max age

  dt[, grp := cut(ALDER, breaks = seq(mix[["min"]],
                                      mix[["max"]],
                                      by = interval),
                  right = FALSE), by = mget(gpv)]

  dt[, ageid := .GRP, by = c(gpv, "grp")]

  vals <- grep("^VAL", names(dt), value = TRUE)
  for (i in vals){
    vai <- tolower(i)
    dt[, (vai) := sum(get(i), na.rm = TRUE), by = ageid]
    dt[, (i) := get(vai)]
    dt[, (vai) := NULL]
  }

  dt <- dt[, .SD[1], by = ageid]

  delVals <- c("grp", "ageid")
  dt[, (delVals) := NULL]
  return(dt)
}
