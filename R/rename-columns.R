#' @title Rename Columns
#' @description
#' Rename standard columns in the dataset
#' @inheritParams do_split
#' @param cols Old and new columns. See output from `get_colname()`
#' @export

do_colname <- function(dt = NULL, cols = NULL) {
  is_debug()
  is_null(dt)
  is_null(cols)

  if (length(cols) == 2) {
    data.table::setnames(dt, old = cols[["old"]], new = cols[["new"]])
  }
  return(dt)
}

#' @title Get Renamed Columns
#' @description
#' Get the old and new coloumnames to be renamed in the dataset.
#' @inheritParams read_raw
#' @inheritParams find_spec
#' @inheritParams get_split
#' @return A list consist of two variables ie. `old` and `new`
#'  indicating the old and new columnames
#' @export

get_colname <- function(group = NULL, con = NULL, spec = NULL) {
  is_null_both(group, spec)
  is_not_null_both(group, spec)

  if (is.null(spec)) {
    spec <- find_spec(file = "filegroups.sql", value = group, con = con)
  }

  input <- find_column_input(spec = spec, "KOLNAVN")

  if (!is.na(input)) {
    input <- is_col_separate(input = input)
  }
  return(input)
}

## Helper funciton ------------------------------
