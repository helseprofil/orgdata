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
  variable <- NULL
  is_debug()

  idvar <- setdiff(names(dt), widecols)
  dt <- data.table::melt(data = dt,
                         id.vars = idvar,
                         measure.vars = widecols,
                         value.name = resval,
                         variable.name = "variable")

  resNr <- length(rescol)
  brc <- is_bracket(resNr)

  for (i in seq_len(resNr)){
    col <- rescol[i]
    subChoose <- paste0("\\", i)
    dt[, (col) := sub(brc, subChoose, variable)]
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

  forCols <- paste0(resCol, collapse = "+")

  ## TODO select only specific folder as reshape id instead of all with ...
  dt <- data.table::dcast(data = dt,
                          formula = paste0("...", "~", forCols),
                          value.var = resVal,
                          sep = ";")
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
  cols <- NULL

  if (length(col) == 1){
    wideCols <- as.character(unique(dt[[col]]))
  } else {
    vals <- vector(mode = "list", length = length(col))
    for (i in seq_along(col)){
      selCol <- col[i]
      val <- as.character(unique(dt[[selCol]]))
      vals[[i]] <- val
    }

    idx <- do.call(data.table::CJ, vals)
    idCols <- names(idx)
    refCols <- data.table::copy(idCols)

    for (i in seq_len(nrow(idx))){
      idCols <- data.table::copy(refCols)
      idx[i, cols := paste0(.SD, collapse = ";"), .SDcols = idCols]
    }
    wideCols <- idx[["cols"]]
  }

  return(wideCols)
}


is_bracket <- function(x){
  # x : Length of reshape columns
  b1 <- "^(.*)"
  b2 <- ";(.*)"

  if (x > 1){
    b3 <- paste0(rep(b2, x - 1), collapse = "")
    b4 <- paste0(b1, b3, collapse = "")
  } else {
    b4 <- b1
  }

  return(b4)
}

