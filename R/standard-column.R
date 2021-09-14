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


#' @title Get Standard Columns
#' @description Standard columns names in rawdata to will be checked against
#'    the standard names in options as in `getOptions("orgdata.columns")`.
#' @inheritParams read_raw
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

  vars <- getOption("orgdata.columns")
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
    is_stop("Can't find column:", var = cols)
  }
}

## TODO How to implement MANHEADER
