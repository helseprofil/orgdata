# Get argument input ------------------------------------------
# When DB column has many input e.g INNLESARG and EXTRA column
#' @title Columns with Multiple Inputs
#' @description Get all arguments in a selected column that have multiple
#' arguments with [find_column_multi()]. The output will be a character
#' type of arguments length. This can then can be used in [find_column_multi_input()]
#' function to get the value of the input as a list object. If you are only
#' interested in a specific argument among these arguments in the column,
#' then use [find_column_multi_input_arg()] function. See example.
#' @param sep Symbols that seperate these arguments eg. "," or ":"
#' @inheritParams find_column_input
#' @examples
#' \dontrun{
#' args <- find_column_multi(spec$INNLESARG)
#' vals <- find_column_multi_input(args)
#' val <- find_column_multi_input_arg(args, "header")
#' }
#' @return Two different output:
#' \itemize{
#'    \item{A vector of the argument that is seperated with \code{sep} argument}
#'    \item{A list of argument names and their values}
#' }
#' @export
find_column_multi <- function(df = NULL, col = NULL, sep = c(",", ":", ";")) {
  # Output is a vector of the INNLESARG input
  check_null(df)
  sep <- match.arg(sep)

  if (is.null(col)) {
    args <- seperate_value(df, sep)
  } else {
    args <- seperate_value(df[, col], sep)
  }
  return(args)
}

#' @export
#' @rdname find_column_multi
#' @param input Input argument(s) as a vector
find_column_multi_input <- function(input = NULL) {
  check_null(input)

  inVar <- input[!is.na(input)]
  outVar <- vector(mode = "list", length = length(inVar))

  for (i in seq_len(length(input)))
  {
    arg <- seperate_value(input[i], "=")
    names(outVar)[i] <- arg[1]
    val <- make_logical(arg[2])
    outVar[[i]] <- val
  }

  return(outVar)
}

#' @export
#' @rdname find_column_multi
#' @param arg Name of a specific argument in the column
#' @inheritParams find_column_multi_input
#' @return The single object with value from the selected argument
find_column_multi_input_arg <- function(input = NULL, arg = NULL) {
  # arg : Name of arg in the column eg. header
  # input : the set of arguments in the columns INNLESARG as a vector via find_column_multi()
  check_null(arg)
  check_null(input)
  one_arg_only(arg)

  i <- grep(paste0("^", arg), input)
  input <- seperate_value(input[i], "=", 2)
  input <- make_logical(input)
  return(input)
}

one_arg_only <- function(x) {
  symbol <- gregexpr("=", x)
  many <- length(symbol[[1]])
  if (many > 1) {
    stop("Too many arguments. Try `find_column_multi_inputs` function")
  }
}

make_logical <- function(x) {
  if (x %in% c("TRUE", "FALSE")) {
    x <- as.logical(x)
  }
  return(x)
}


seperate_value <- function(x, sep = NULL, keep = NULL) {
  # keep : Keep lhs or rhs eg. x[1] for lhs
  out <- unlist(strsplit(x, sep))
  if (!is.null(keep)) {
    out <- out[keep]
  }

  out <- trimws(out)
  return(out)
}
