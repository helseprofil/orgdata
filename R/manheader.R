#' @title Rename Colname Manually
#' @description Rename the columns manually based on column index.
#' @inheritParams do_split
#' @param manspec Specification from \code{MANHEADER} column in \code{tbl_Innlesing}
#' @import data.table
#' @export
do_manheader <- function(dt = NULL, manspec = NULL) {
  is_null(dt, "Data set not found!")
  is_null(manspec)

  check <- length(manspec)
  if (check == 2) {
    nameIndex <- manspec[["index"]]
    data.table::setnames(dt, names(dt)[nameIndex], manspec[["col"]])
  }
  return(dt)
}


#' @title Get MANHEADER
#' @description Get the inputs for column MANHEADER in \code{tbl_Innlesing}.
#'    The input tells that we want to manually \strong{rename} the column for
#'    various reasons such as column name in the rawdata is too long or
#'    it uses unstandard naming style. An input like this:
#'
#' \code{3,6=AGE,EDUCATION}
#'
#' means we want to rename column \code{3} to \code{AGE} and column \code{6}
#' to \code{EDUCATION}.
#' @inheritParams read_raw
#' @inheritParams find_spec
#' @inheritParams find_column_input
#' @return A list containing \code{$index} to refer to the column index and
#'     \code{$col} for the new name of the selected column index.
#' @export
get_manheader <- function(group = NULL, con = NULL, spec = NULL) {
  is_null_both(group, spec)
  is_not_null_both(group, spec)

  if (is.null(spec)) {
    spec <- find_spec("specification.sql", group, con)
  }

  input <- find_column_input(spec, "MANHEADER")

  if (!is.na(input)) {
    args <- is_separate(input, "=")
    lhs <- is_separate(args[1], ",")
    ## lhs will always be int as it refers to column number in rawdata
    lhs <- as.integer(lhs)
    rhs <- is_separate(args[2], ",")
    input <- list(index = lhs, col = rhs)
  }
  return(input)
}
