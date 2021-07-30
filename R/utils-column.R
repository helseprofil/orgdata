
#' Columns with Single Input
#' @description
#' Get the value of a column when it's only one input is allowed.
#' @param df Data.frame extracted form registration database
#' @param type Type of object as output ie. double, integer or character.
#'     Default is character.
#' @param col Column name in the database table
#' @export
find_column_input <- function(df, col, type = c("character", "double", "integer")) {
  # df : Input data as data.frame
  # col : Selected column in df
  # type : type of input object will be checked with typeof()
  type <- match.arg(type)
  val <- trimws(df[, col])
  val <- input_type(val, type)
  val <- make_logical(val)
  return(val)
}


#' @keywords internal
#' @title Type of object input
#' @description Convert value to selected type ie. checked with [typeof]
#' @param value Input value to be converted
input_type <- function(value, type = c("character", "double", "integer")) {
  # value : value to be converted
  # type : type of input object will be checked with typeof()
  type <- match.arg(type)
  val <- switch(type,
    double = as.numeric(value),
    integer = as.integer(value),
    character = as.character(value)
  )
}
