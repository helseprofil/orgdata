#' @title Special Need Reshape to Long
#' @description This function is only applicable to reshape data that was
#'   reshaped to wide via Access specification in `RESHAPE` columns.
#' @param dt An output dataset from `do_reshape_wide()`
#' @param resval Column value to be reshaped from
#' @param rescol Column(s) dimension where the value derived from
#' @param widecols Column(s) names created for wide reshape in the dataset
#' @family reshape functions
#' @export
do_reshape_long <- function(dt, resval, rescol, widecols){

  is_debug()

  idvar <- setdiff(names(dt), widecols)
  dt <- data.table::melt(data = dt,
                         id.vars = idvar,
                         measure.vars = widecols,
                         value.name = resval,
                         variable.name = "variable")

  if (length(rescol) > 3){
    is_stop("Too many reshape columns! Max is 3 columns")
  }

  ## TODO Need to refactor this! Too many repeatition!
  if (length(rescol) == 3){
    col1 <- rescol[1]
    col2 <- rescol[2]
    col3 <- rescol[3]
    dt[, (col1) := sub("(.*);(.*);(.*)", "\\1", variable)]
    dt[, (col2) := sub("(.*);(.*);(.*)", "\\2", variable)]
    dt[, (col3) := sub("(.*);(.*);(.*)", "\\3", variable)]
  }

  if (length(rescol) == 2){
    col1 <- rescol[1]
    col2 <- rescol[2]
    dt[, (col1) := sub("(.*);(.*)", "\\1", variable)]
    dt[, (col2) := sub("(.*);(.*)", "\\2", variable)]
  } else {
    dt[, (rescol) := variable]
  }

  dt[, variable := NULL]
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

  dt <- copy(dt)
  resCol <- respec$rescol
  resVal <- respec$resval

  ## TODO select only specific folder as reshape id instead of all with ...
  ## TODO Need to refactor this! Too many repeatition!
  if (length(resCol) == 3){
    dt <- data.table::dcast(data = dt,
                            formula = paste0("...", "~", resCol[1], "+", resCol[2], "+", resCol[3]),
                            value.var = resVal, sep = ";")
  }

  if (length(resCol) == 2){
    dt <- data.table::dcast(data = dt,
                            formula = paste0("...", "~", resCol[1], "+", resCol[2]),
                            value.var = resVal, sep = ";")
  } else {
    dt <- data.table::dcast(data = dt,
                            formula = paste0("...", "~", resCol),
                            value.var = resVal)
  }

  return(dt)
}

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
  wideCols <- is_reshape_wide_cols(dt, resCol)

  return(list(rescol = resCol,
              resval = resVal,
              widecols = wideCols))
}

## Helper ---------------------
## All possible combination of unique vectors to create value columns
## from when dt is turned to wide especially with multiple reshape columns
is_reshape_wide_cols <- function(dt, col){
  # col - Value columns in dataset when turn to wide

  ## TODO Need to refactor this! Too many repeatition!
  if (length(col) == 3){
    col1 <- col[1]
    col2 <- col[2]
    col3 <- col[3]
    colVal1 <- as.character(unique(dt[[col1]]))
    colVal2 <- as.character(unique(dt[[col2]]))
    colVal3 <- as.character(unique(dt[[col3]]))
    idx <- data.table::CJ(colVal1, colVal2, colVal3)
    idx[, cols := paste0(colVal1, ";", colVal2, ";", colVal3)]
    wideCols <- idx[["cols"]]
  }

  if (length(col) == 2){
    col1 <- col[1]
    col2 <- col[2]
    colVal1 <- as.character(unique(dt[[col1]]))
    colVal2 <- as.character(unique(dt[[col2]]))
    idx <- data.table::CJ(colVal1, colVal2)
    idx[, cols := paste0(colVal1, ";", colVal2)]
    wideCols <- idx[["cols"]]
  } else {
    wideCols <- as.character(unique(dt[[col]]))
  }

  return(wideCols)
}
