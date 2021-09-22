#' @title Create Implicit NULL
#' @description All variables except `AAR` and `GEO` must have equal number of
#'   categories. The data that is handled by this package is an aggregated
#'   rawdata. The categories for each variables might be different in different
#'   year. This does not elucidate non-existence of the category but rather a
#'   zero number belonging to that category. This function will standardize the
#'   categories across all year.
#' @param dt Dataset consisting all years
#' @export
do_implicit_null <- function(dt){

  ignoreCol <- c("GEO", "AAR", "VAL1")
  cols <- setdiff(names(dt), ignoreCol)



}


## dt - dataset
## var - Colname to be checked
## year - Colname for year
is_implicit_null <- function(dt, year, var){
  var <- as.character(deparse(substitute(var)))
  year <- as.character(deparse(substitute(year)))
  ref <- sort(unique(dt[[var]]))
  aar <- sort(unique(dt[[year]]))
  nn <- vector(mode = "list", length = length(aar))

  for (i in seq_along(aar)){
    yr <- aar[i]
    vars <- sort(unique(dt[[var]][dt[[year]] == yr]))
    dd <- setdiff(ref, vars)
    nn[[i]] <- dd
    names(nn)[i] <- as.character(yr)
  }
  invisible(nn)
}
