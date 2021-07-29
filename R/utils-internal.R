# Check code ------------------------------------------
# SQL code need sprintf for dynamic query
check_sql <- function(x) {
  # x : file with sql code
  if (grepl("%", x) != 1) {
    stop("Missing `sprintf` reference in SQL code")
  }
}

# Arguments that are missing
check_null <- function(arg, msg = NULL) {
  # msg : Alternative message if not using default
  argchr <- deparse(substitute(arg))

  if (!is.null(msg)) {
    msgTxt <- msg
  } else {
    msgTxt <- sprintf("Argument for `%s` is missing", argchr)
  }

  if (is.null(arg)) {
    stop(msgTxt)
  }
}

# Standard dummy input starts with symbol $
# eg. column AAR
dummy_input <- function(x) {
  grepl("^\\$", x)
}

# To seperate a string with selected sep symbol
# Can keep both value ie. lhs and rhs or either one
seperate_value <- function(x, sep = NULL, keep = NULL) {
  # x : the string to be seperated
  # sep : seperate symbole like ",","=" etc.
  # keep : Keep lhs or rhs eg. x[1] for lhs
  out <- unlist(strsplit(x, sep))
  if (!is.null(keep)) {
    out <- out[keep]
  }

  out <- trimws(out)
  return(out)
}
