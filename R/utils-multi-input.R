# Get argument input ------------------------------------------
# When DB column has many input e.g INNLESARG and EXTRA column
#' @title Columns with Multiple Inputs
#' @description Get all arguments in a selected column that have multiple
#' arguments with [find_column_multi()]. The output will be a character
#' type of arguments length. This can then can be used in [find_column_multi_input()]
#' function to get the value of the input as a list object. If you are only
#' interested in a specific argument among these arguments in the column,
#' then use [find_column_multi_input_arg()] function. See example.
#' @param sep Symbols that separate these arguments eg. "," or ":"
#' @inheritParams find_column_input
#' @examples
#' \dontrun{
#' args <- find_column_multi(spec, "INNLESARG")
#' vals <- find_column_multi_input(args)
#' val <- find_column_multi_input_arg(args, "header")
#' }
#' @return Output:
#' \itemize{
#'    \item{`find_column_multi` gives a character vector of the
#'          arguments that is separated with \code{sep} argument}
#'    \item{`find_column_multi_input` gives a list of argument names and their values}
#'    \item{`find_column_multi_input_arg` gives a single object with value from the selected argument}
#' }
#' @export
find_column_multi <- function(spec = NULL, col = NULL, sep = c(",", "|", ":", ";")) {
  # Output is a vector of the INNLESARG input
  is_bugs()
  is_null(spec)
  sep <- match.arg(sep)

  if (is.null(col)) {
    args <- is_separate(spec, sep)
  } else {
    args <- is_separate(spec[, col], sep)
  }
  return(args)
}

#' @export
#' @rdname find_column_multi
#' @param input Input argument(s) as a character vector
find_column_multi_input <- function(input = NULL) {
  is_bugs()
  is_null(input)

  inVar <- input[!is.na(input)]
  outVar <- vector(mode = "list", length = length(inVar))

  for (i in seq_len(length(inVar)))
  {
    arg <- is_separate(inVar[i], "=")
    names(outVar)[i] <- arg[1]
    val <- is_logical(arg[2])
    outVar[[i]] <- val
  }

  return(outVar)
}

#' @export
#' @rdname find_column_multi
#' @param arg Name of a specific argument in the column
#' @inheritParams find_column_multi_input

find_column_multi_input_arg <- function(input = NULL, arg = NULL) {
  # arg : Name of arg in the column eg. header
  # input : the set of arguments in the columns INNLESARG as a vector via find_column_multi()
  is_null(arg)
  is_null(input)
  is_one_arg(arg)

  i <- grep(paste0("^", arg), input)
  input <- is_separate(input[i], "=", 2)
  input <- is_logical(input)
  return(input)
}

is_one_arg <- function(x) {
  symbol <- gregexpr("=", x)
  many <- length(symbol[[1]])
  if (many > 1) {
    stop("Too many arguments. Try `find_column_multi_inputs` function")
  }
}
