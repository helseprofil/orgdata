# Check code ------------------------------------------
# SQL code need sprintf for dynamic query
check_sql <- function(x) {
  if (grepl("%", x) != 1) {
    stop("Missing sprintf reference in SQL code")
  }
}

# Arguments that are missing
check_null <- function(arg, msg = NULL) {
  # msg : Alternative message if not using default
  argchr <- deparse(substitute(arg))

  if (!is.null(msg)) {
    msgTxt <- msg
  } else {
    msgTxt <- sprintf("Argument for %s is missing", argchr)
  }

  if (is.null(arg)) {
    stop(msgTxt)
  }
}

# Get value from column with only one input -------------------------
column_value <- function(df, col, type) {
  # df : Input data as data.frame
  # col : Selected column in df
  # type : type of input object will be checked with typeof()
  val <- trimws(df[col])
  val <- column_type(val, type)
}

column_type <- function(value, type = c("double", "integer", "character")) {
  # value : value to be converted
  # type : type of input object will be checked with typeof()
  type <- match.arg(type)
  val <- switch(type,
                double = as.numeric(value),
                integer = as.integer(value),
                character = as.character(value)
                )
}
