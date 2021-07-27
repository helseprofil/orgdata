# Get argument input ------------------------------------------
# When DB column has many input e.g INNLESARG and EXTRA column
#' @title Columns with Multiple Arguments
#' @description Get all arguments in the columns that have multiple arguments with [get_column_multi_args()].
#' The output will be a vector that can be used in [orgdata::get_input_multi_args()] to get the value of the input.
#' @param sep Symbols that seperate these arguments eg. "," or ":"
#' @inheritParams get_innlesarg
#' @examples
#' \dontrun{
#' args <- get_column_multi_args(spec$INNLESARG)
#' val <- get_input_multi_args("header", args)
#' }
#' @export
get_column_multi_args <- function(col = NULL, sep = c(",", ":", ";")) {
  # col : colum name in Access DB
  # sep = , : INNLESARG
  # sep = | : EXTRA
  # Output is a vector of the INNLESARG input
  check_null(col)
  sep <- match.arg(sep)
  args <- unlist(strsplit(col, sep))
  args <- trimws(args)
}

#' @export
#' @rdname get_column_multi_args
#' @param y Argument(s) as a vector
get_input_multi_args <- function(arg = NULL, y = NULL) {
  # arg : Name of arg in the column eg. header
  # y : the set of arguments in the columns INNLESARG as a vector via get_column_multi_args()
  check_null(arg)
  check_null(y)
  i <- grep(paste0("^", arg), y)
  input <- unlist(strsplit(y[i], "="))[2]

  if (input %in% c("TRUE", "FALSE")) {
    input <- as.logical(input)
  }
  input
}
