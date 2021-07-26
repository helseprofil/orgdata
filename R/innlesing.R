#' Get File Specifications
#'
#' @description
#' This function will access all specifications on how
#' the data will be clean, restructured and aggregated. These specifications are registered in
#' the following register database tables:
#' \enumerate{
#'   \item{tbl_Filgruppe}
#'   \item{tbl_Orgfile}
#'   \item{tbl_Innlesing}
#' }
#'
#' @param arg Name of the arguments
#' @param col Column name in the table
#' @param ... Other arguments eg. sep = ":"
#' @examples
#' \dontrun{
#' input <- get_innlesarg("ark", spec$INNLESARG)
#' }
#' @return A character
#' @export

get_innlesarg <- function(arg, col, ...) {
  args <- get_column_multi_args(col, ...)
  input <- get_input_multi_args(arg, args)
}
