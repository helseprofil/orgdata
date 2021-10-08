#' @title Rename Standard Columns
#' @description Renaming standard columns as in `getOption("orgdata.columns")`.
#' @inheritParams do_split
#' @param spec Specification data as list. See output from \code{get_column_standard}
#' @export
do_column_standard <- function(dt = NULL, spec = NULL) {
  is_debug()
  is_null(dt, "Data set not found!")
  is_null(spec, "Specification to rename columns is missing")

  if (isFALSE(is.list(spec))) {
    stop("Input for `spec` must be a 'list' with `old` and `new` names")
  }

  is_check_cols(x = spec$old, y = names(dt))
  data.table::setnames(dt, spec$old, spec$new)
}

is_column_equal <- function(dt, spec){

}

#' @title Get Standard Columns
#' @description Standard columns names in rawdata will be checked against
#'    the standard names in options as in `getOptions("orgdata.columns")`.
#'    Nevertheless column `GEO` is a special case when geo codes are derived
#'    from a combination of two columns.
#' @inheritParams make_file
#' @inheritParams find_spec
#' @param spec Specification of the standard columns in \code{tbl_Innlesing}
#' @return A list with `old` and `new` columnnames
#' @import data.table
#' @export
get_column_standard <- function(group = NULL, con = NULL, spec = NULL) {

  is_null_both(group, spec)
  is_not_null_both(group, spec)

  if (is.null(spec)) {
    spec <- find_spec("filegroups.sql", group, con)
  }

  VARS <- getOption("orgdata.columns")
  vars <- VARS[is.element(VARS, names(spec))]

  ## When GEO is a combination of two or more columns
  geoVals <- is_separate(spec$GEO, ",")
  if (length(geoVals) > 1){
    vars <- vars[vars != "GEO"]
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
  dummy <- is_dummy(input)
  if (dummy) input <- NA
  list(col = col, input = input)
}

is_check_cols <- function(x, y){
  ## x - the defined cols in register database
  ## y - the existing cols in dt
  cols <- setdiff(x, y)
  if (length(cols) > 0){
    message("Column names: ", paste_cols(y))
    message("Access specs: ", paste_cols(x))
    is_stop("Column name(s) in Access specs are different than in the dataset!")
  }
}

paste_cols <- function(cols){
  paste0('"', paste(cols, collapse = '", "'), '"')
}

## TODO How to implement MANHEADER
