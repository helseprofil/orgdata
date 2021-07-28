# Get argument input ------------------------------------------
# When DB column has many input e.g INNLESARG and EXTRA column
#' @title Columns with Multiple Inputs
#' @description Get all arguments in the columns that have multiple arguments with [find_column_multi()].
#' The output will be a vector that can be used in [orgdata::find_column_multi_input()] to get the value of the input.
#' @param sep Symbols that seperate these arguments eg. "," or ":"
#' @inheritParams find_column_input
#' @examples
#' \dontrun{
#' args <- find_column_multi(spec$INNLESARG)
#' val <- find_column_multi_input("header", args)
#' }
#' @return A vector of the argument(s) that is seperated with \code{sep} argument
#' @export
find_column_multi <- function(col = NULL, sep = c(",", ":", ";")) {
  # Output is a vector of the INNLESARG input
  check_null(col)
  sep <- match.arg(sep)
  args <- unlist(strsplit(col, sep))
  args <- trimws(args)
}

#' @export
#' @rdname find_column_multi
#' @param arg Name of argument
#' @param y Argument(s) as a vector
#' @return The value from the selected argument
find_column_multi_input <- function(arg = NULL, y = NULL) {
  # arg : Name of arg in the column eg. header
  # y : the set of arguments in the columns INNLESARG as a vector via find_column_multi()
  check_null(arg)
  check_null(y)
  i <- grep(paste0("^", arg), y)
  input <- unlist(strsplit(y[i], "="))[2]

  if (input %in% c("TRUE", "FALSE")) {
    input <- as.logical(input)
  }
  input
}
