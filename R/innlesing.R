#' Get INNLESARG Specifications
#'
#' @description
#' This function will access all arguments in columns \code{INNLESARG} in table
#' \emph{tbl_Innlesing}' where there are possibility to
#' specify multiple arguments for example:
#' `header=TRUE, ark=Sheet1`
#' @param arg Name of the arguments
#' @param ... Other arguments eg. sep = ":"
#' @inheritParams get_column_input
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

#' @title Get year
#' @description
#' Get the year value in column \code{DEFAAR} from table \emph{tbl_Orgfile}
#' when column for year isn't available in the raw data. This is
#' indicated with \code{$Y} in the table \emph{ tbl_Innlesing }.
#' @param id \code{FILID} from table \emph{tbl_Orgfile}
#' @inheritParams read_spec
#' @return An integer
#' @export
get_year_from_file <- function(id = NULL, con = NULL) {
  check_null(id, "FILID is missing")
  check_null(con)
  df <- read_spec("file-year.sql", id, con)
  val <- get_column_input(df, "DEFAAR", "int")
}

#' @export
#' @rdname get_year_from_file
#' @param df Input data as data.frame
#' @inheritParams get_innlesarg
get_year <- function(df = NULL, col = NULL) {
  check_null(col)
  get_column_input(df, "AAR", "int")
}
