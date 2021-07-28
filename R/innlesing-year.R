

#' @title  Year for The Data
#' @description
#' Find the year value in column \code{DEFAAR} from table \emph{tbl_Orgfile}
#' when column for year isn't available in the raw data. This is
#' indicated with \code{$Y} in column \code{AAR} in table \emph{tbl_Innlesing}.
#' If column \code{ AAR } has other input then use function [find_year_from_column()] instead.
#' @param id \code{FILID} from table \emph{tbl_Orgfile}
#' @inheritParams find_spec
#' @return An integer
#' @export
find_year <- function(id = NULL, con = NULL) {
  check_null(id, "FILID is missing")
  check_null(con)
  df <- find_spec("file-year.sql", id, con)
  val <- find_column_input(df, "DEFAAR", "int")
}

#' @export
#' @rdname find_year
#' @param df Input data as data.frame
#' @inheritParams get_innlesarg
find_year_from_column <- function(df = NULL, col = NULL) {
  check_null(col)
  find_column_input(df, "AAR", "int")
}
