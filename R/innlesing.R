#' Get INNLESARG Specifications
#'
#' @description
#' This function will access all arguments in columns \code{INNLESARG} in table
#' \emph{tbl_Innlesing}' where there are possibility to
#' specify multiple arguments for example:
#' `header=TRUE, ark=Sheet1`
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

#' @title Get year
#' @description
#' Get the year value in column \code{DEFAAR} from table \emph{tbl_Orgfile}
#' when column for year isn't available in the raw data. This is
#' indicated with \code{$Y} in the table \emph{ tbl_Innlesing }.
#' @param id Is \code{FILID} from table \emph{tbl_Orgfile}
#' @inheritParams read_spec
#' @return An integer
#' @export
get_year_from_file <- function(id = NULL, con = NULL) {
  check_null(id, "FILID is missing")
  check_null(con)
  input <- read_spec("file-year.sql", id, con)
  as.integer(input$DEFAAR)
}
