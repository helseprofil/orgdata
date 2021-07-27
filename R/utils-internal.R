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

