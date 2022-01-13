#' @title Special Need Reshape to Long
#' @description This function is only applicable to reshape data that was
#'   reshaped to wide via Access specification in `RESHAPE` columns.
#' @param dt An output dataset from `do_reshape_wide()`
#' @inheritParams do_reshape_wide
#' @family reshape functions
#' @export
do_reshape_long <- function(dt = NULL, respec = NULL){
  is_debug()

  idvar <- setdiff(names(dt), respec$widecol)
  dt <- data.table::melt(data = dt,
                         id.vars = idvar,
                         measure.vars = respec$widecol,
                         value.name = respec$resval,
                         variable.name = "variable",
                         variable.factor = FALSE)

  data.table::setnames(dt, "variable", respec$rescol)
  return(dt)
}


#' @title Reshape from Long to Wide
#' @description Reshape the dataset from long format to wide format.
#' @param dt Dataset to be reshaped
#' @param respec Reshape specification with `id` and `value` variables. This
#'   is the output from `get_reshape_wide_spec()`
#' @family reshape functions
#' @export
do_reshape_wide <- function(dt = NULL, respec = NULL){

  is_debug()
  is_null(respec)

  dt <- data.table::copy(dt)
  resCol <- respec$rescol
  resVal <- respec$resval

  ## TODO select only specific folder as reshape id instead of all with ...
  dt <- data.table::dcast(data = dt,
                          formula = paste("...", "~", resCol),
                          value.var = resVal)

  return(dt)
}

# Codebook for RESHAPE
# 1 = LONG
# 2 = WIDE

#' @title Reshape Wide Specification
#' @description Get the formula and value variables for reshaping a long dataset
#'   to wide. For detail please read `data.table::dcast.data.table` to
#'   understand `formula` and `value` variables.
#' @inheritParams do_reshape
#' @inheritParams make_file
#' @inheritParams get_split
#' @inheritParams find_column_input
#' @family reshape functions
#' @export
get_reshape_wide_spec <- function(dt = NULL, group = NULL, con = NULL, spec = NULL){

  is_debug()
  is_null_both(group, spec)
  is_not_null_both(group, spec)

  if (is.null(spec)) {
    spec <- find_spec("specification.sql", value = group, con = con)
  }

  ## TODO Delete or exclude column that should not be included as in RESHAPE_ID
  resCol <- find_column_multi(spec, "RESHAPE_KOL")
  resVal <- find_column_input(spec, "RESHAPE_VAL")
  wideCol <- as.character(unique(dt[[resCol]]))
  multTab <- is_multi_wide(dt, spec)

  return(list(rescol = resCol,
              resval = resVal,
              widecol = wideCol,
              multtab = multTab))
}

## Helper ---------------------
## If there are more dimensions to tabulate ie. multiple TABs
## Then get the valid categories for multiple TABs

is_multi_wide <- function(dt, spec){
  # spec - File specification
  tabs <- is_multi_tabs(spec)

  if (length(tabs) > 1){
    dt[, "wideTAB" := do.call(paste0, .SD), .SDcols = tabs]
    cols <- unique(dt$wideTAB)
    dt[, "wideTAB" := NULL]
  } else {
    cols <- NA
  }

  return(cols)
}

is_multi_tabs <- function(spec){
  TAB <- paste0("TAB", 1:getOption("orgdata.tabs"))
  tabs <- sapply(TAB, function(x) find_column_input(spec, x))
  names(tabs[!is.na(tabs)])
}
