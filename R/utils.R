# Check code ------------------------------------------
# SQL code need sprintf for dynamic query
check_sql <- function(x) {
  if (grepl("%", x) != 1) {
    stop("Missing sprintf reference in SQL code")
  }
}

# Arguments that are missing
check_null <- function(arg) {
  argchr <- deparse(substitute(arg))
  if (is.null(arg)) {
    stop("Argument for ", argchr, " is missing")
  }
}


# Get argument input ------------------------------------------
# When DB column has many input e.g INNLESARG and EXTRA column
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
# Example
## (arr <- get_column_multi_args(spec$INNLESARG))

get_input_multi_args <- function(arg = NULL, y = NULL) {
  # arg : Name of arg in the column eg. header
  # y : the set of arguments in the columns INNLESARG as a vector via get_column_multi_args()
  check_null(arg)
  check_null(y)
  i <- grep(paste0("^", arg), y)
  input <- unlist(strsplit(y[i], "="))[2]
}
## Example
## arg <- get_input_multi_args("ark", arr)
