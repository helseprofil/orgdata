#' Get INNLESARG Specifications
#'
#' @description
#' This function will access all possible arguments in columns \code{INNLESARG}
#' in table \emph{tbl_Innlesing}'. Comma \code{","} \strong{MUST} be used to
#' seperate arguments if there are more than one argument.
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
  input <- find_column_multi_input(args)
}
