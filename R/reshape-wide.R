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
  valCols <- is_reshape_wide_cols(dt, resCol)

  return(list(rescol = resCol,
              resval = resVal,
              valcols = valCols))
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
    valCols <- idx[["cols"]]
  }

  if (length(col) == 2){
    col1 <- col[1]
    col2 <- col[2]
    colVal1 <- as.character(unique(dt[[col1]]))
    colVal2 <- as.character(unique(dt[[col2]]))
    idx <- data.table::CJ(colVal1, colVal2)
    idx[, cols := paste0(colVal1, ";", colVal2)]
    valCols <- idx[["cols"]]
  } else {
    valCols <- as.character(unique(dt[[col]]))
  }

  return(valCols)
}
