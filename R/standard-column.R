#' @title Rename Standard Columns
#' @description Renaming standard columns. To see what are the standard columns
#'   run `orgdata:::is_standard_cols()`.
#' @inheritParams do_split
#' @param spec Specification data as list. See output from
#'   \code{get_column_standard}
#' @export
do_column_standard <- function(dt = NULL, spec = NULL) {
  is_debug()
  is_null(dt, "Data set not found!")
  is_null(spec, "Specification to rename columns is missing")

  if (isFALSE(is.list(spec))) {
    stop("Input for `spec` must be a 'list' with `old` and `new` names")
  }

  data.table::setnames(dt, names(dt), toupper(names(dt)))
  # Ensure older spec with rigit columnames still work
  spec$old <- toupper(spec$old)

  is_check_cols(x = spec$old, y = names(dt))
  data.table::setnames(dt, spec$old, spec$new)
}


#' @title Get Standard Columns
#' @description Standard columns names in rawdata will be checked against
#'    the standard names in options as in `getOption("orgdata.columns")`.
#'    Nevertheless column `GEO` is a special case when geo codes are derived
#'    from a combination of two columns.
#' @inheritParams make_file
#' @inheritParams find_spec
#' @param spec Specification of the standard columns in \code{tbl_Innlesing}
#' @return A list with `old` and `new` columnnames
#' @import data.table
#' @export
get_column_standard <- function(group = NULL, con = NULL, spec = NULL) {
  is_debug(deep = TRUE)
  is_null_both(group, spec)
  is_not_null_both(group, spec)

  if (is.null(spec)) {
    spec <- find_spec("filegroups.sql", group, con)
  }

  VARS <- is_standard_cols()
  vars <- VARS[is.element(VARS, names(spec))]

  ## When GEO is a combination of two or more columns
  geoVals <- is_separate(spec$GEO, ",")
  if (length(geoVals) > 1){
    vars <- vars[vars != "GEO"]
  }

  ## When RESHAPE is 2 = LONG exclude TAB1 and VAL1
  ## but still need to get which columns are defined since it can be few VALs
  reshcol <- spec$RESHAPE
  if (!is.na(reshcol) && reshcol == 1){
    vars <- is_reshape_col(vars, spec)
  }

  for (i in seq_along(vars)) {
    input <- is_column_na(spec, vars[i])
    assign(input[["col"]], input)
  }
  x <- data.table::rbindlist(mget(vars))

  old <- x[!is.na(x$input), ]$input
  new <- x[!is.na(x$input), ]$col
  list(old = old, new = new)
}


## Change dummy input to NA for easy selection
is_column_na <- function(spec, col) {
  input <- find_column_input(spec, col)
  dummy <- is_col_dummy(input)
  if (dummy) input <- NA
  list(col = col, input = input)
}

is_check_cols <- function(x, y){
  ## x - the defined cols in register database
  ## y - the existing cols in dt
  cols <- setdiff(x, y)
  if (length(cols) > 0){
    ## message(crayon::green("Column names: "), paste_cols(y))
    ## message(crayon::green("Access specs: "), paste_cols(x))
    is_colour_txt(x = paste_cols(y), msg = "Column names:", type = "note")
    is_colour_txt(x = paste_cols(x), msg = "Access specs:", type = "note")
    is_stop("Column name(s) in Access specs are different than in the dataset!")
  }
}

## Find RESHAPE_KOL to exclude from standard
is_reshape_col <- function(vars, spec){
  ## vars - standard variables or columnames
  reshVars <- is_col_separate(spec$RESHAPE_KOL)$old

  reshCols <- c("TAB1", paste0("VAL", 1:getOption("orgdata.vals")))
  reshNo <- setdiff(reshVars, reshCols)
  if (length(reshNo) != 0){
    is_stop("Check RESHAPE_KOL with variable:  ", reshNo)
  }

  rsh <- intersect(reshVars, vars)
  if (length(rsh) > 0){
    ## varx <- intersect(reshVars, vars)
    vars <- setdiff(vars, reshVars)
  }

  return(vars)
}
