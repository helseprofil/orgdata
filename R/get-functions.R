#' Get INNLESARG Specifications
#'
#' @description
#' This function will access all possible arguments in columns \code{INNLESARG}
#' in table \emph{tbl_Innlesing}'. Comma \code{","} \strong{MUST} be used to
#' separate arguments if there are more than one argument.
#' For example:
#'
#' `header=TRUE, ark=Sheet1`
#' @inheritParams get_year
#' @inheritParams find_spec
#' @examples
#' @return A list with the names and value of arguments
#' @export
get_innlesarg <- function(df = NULL) {
  # column INNLESARG must use coma ',' as seperation btw arguments!
  args <- find_column_multi(df, "INNLESARG", sep = ",")
  find_column_multi_input(args)
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
#' @inheritParams get_year
#' @return A list containing \code{$index} to refer to the column index and
#'     \code{$col} for the new name of the selected column index.
#' @export
get_manheader <- function(df = NULL) {
  input <- find_column_input(df, "MANHEADER")
  args <- separate_value(input, "=")
  lhs <- separate_value(args[1], ",")
  rhs <- separate_value(args[2], ",")

  return(list(index = lhs, col = rhs))
}


#' @title Get Column Input
#' @description Get the real column name in rawdata if it's not manually changed
#'   as specified in the column \code{MANHEADER}. Only applicable for single input, else
#'   check [find_column_multi()] or [get_innlesarg()] function.
#' @inheritParams find_column_input
#' @examples
#' \dontrun{
#' geo <- get_column_input(df, "GEO")
#' }
#' @export
get_column_input <- function(df, col) {
  find_column_input(df, col)
}
