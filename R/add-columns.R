#' @title Add Columns
#' @description
#' Add columns to the dataset
#' @inheritParams do_split
#' @param cols Old and new columns. See output from `get_addcols()`
#' @export

do_addcols <- function(dt = NULL, cols = NULL){
  is_null(dt)
  is_null(cols)

  if (length(cols) == 2) {
    data.table::setnames(dt, old = cols[["old"]], new = cols[["new"]])
  }
  return(dt)
}

#' @title Get New Columns
#' @description
#' Get the old and new coloumnames to be added to the dataset.
#' @inheritParams read_org
#' @inheritParams find_spec
#' @inheritParams get_split
#' @return A list consist of two variables ie. `old` and `new`
#'  indicating the old and new columnames
#' @export

get_addcols <- function(group = NULL, con = NULL, spec = NULL){

  is_null_also(group, spec)
  is_null_both(group, spec)

  if (is.null(spec)){
    spec <- find_spec(file = "filegroups.sql", value = group, con = con)
  }

  input <- find_column_input(spec = spec, "ADDKOL")

  if (!is.na(input)){
    args <- is_separate(input, "=")
    lhs <- is_separate(args[1], ",")
    rhs <- is_separate(args[2], ",")
    input <- list(old = lhs, new = rhs)
  }
  return(input)
}
