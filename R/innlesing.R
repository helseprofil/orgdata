#' Get INNLESARG Specifications
#'
#' @description
#' This function will access all arguments in columns \code{INNLESARG} in table
#' \emph{tbl_Innlesing}' where there are possibility to
#' specify multiple arguments for example:
#' `header=TRUE, ark=Sheet1`
#' @param arg Name of the arguments
#' @param ... Other arguments eg. sep = ":"
#' @inheritParams find_column_input
#' @examples
#' \dontrun{
#' input <- get_innlesarg("ark", spec$INNLESARG)
#' }
#' @return A character
#' @export
get_innlesarg <- function(arg, col, ...) {
  args <- find_column_multi(col, ...)
  input <- find_column_multi_input(arg, args)
}
