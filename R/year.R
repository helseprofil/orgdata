#' @title Add Year to Data
#' @description Add column for year if it doesn't exist in the rawdata
#' @inheritParams do_split
#' @param year Output from [get_year()]
#' @import data.table
#' @export
do_year <- function(dt = NULL, year = NULL) {
  AAR <- NULL
  is_null(dt)
  is_null(year)

  var <- year[["year"]]
  if (year[["dummy"]] == 1) {
    dt[, AAR := var]
  }

  return(dt)
}


#' @title Get Year of Data
#' @description
#' Get the year of the rawdata either when it's available in the rawdata or
#' manually specified in the registration database. Check helper function [is_defaar()].
#'
#' [get_aar()] is an alias of [get_year()]
#' @param spec Specifications data as a data.frame
#' @inheritParams get_innlesarg
#' @inheritParams find_spec
#' @return Either a character or integer value.
#'    \itemize{
#'        \item{\code{Character} value is refering
#'              to the column name in the raw data that has value for year.}
#'        \item{\code{Integer} value refers to the year that shoule be added
#'               to the raw data with column name \code{AAR}}
#'    }
#' @aliases get_year get_aar
#' @export
get_year <- function(spec = NULL, con = NULL) {
  year <- find_column_input(spec, "AAR")
  dummy <- is_dummy(year)

  if (dummy) {
    fileID <- find_column_input(spec, "FILID", "int")
    year <- is_defaar(fileID, con)
  }

  return(list(year = year, dummy = dummy))
}


#' @export
#' @rdname get_year
get_aar <- get_year


#' @keywords internal
#' @title  Year for The Data
#' @description
#' Find the year value in column \code{DEFAAR} from table \emph{tbl_Orgfile}
#' when column for year isn't available in the raw data. This is
#' indicated with \code{$Y} in column \code{AAR} in table \emph{tbl_Innlesing}.
#' @inheritParams find_spec
#' @inheritParams read_raw
#' @return An integer
is_defaar <- function(id = NULL, con = NULL) {
  is_null(id, "FILID is missing")
  is_null(con)
  spec <- find_spec("file-year.sql", id, con)
  find_column_input(spec, "DEFAAR", "int")
}
