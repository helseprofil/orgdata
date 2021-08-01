# Check code ------------------------------------------
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
# eg. column AAR with $Y
dummy_input <- function(x) {
  grepl("^\\$", x)
}

# To separate a string with selected sep symbol
# Can keep both value ie. lhs and rhs or either one
separate_value <- function(x, sep = NULL, keep = NULL) {
  # x : the string to be separated
  # sep : separate symbole like ",","=" etc.
  # keep : Keep lhs or rhs eg. x[1] for lhs
  check_null(sep)
  out <- unlist(strsplit(x, sep))
  if (!is.null(keep)) {
    out <- out[keep]
  }

  trimws(out)
}

# TRUE/FALSE is character when fetching data from DB
make_logical <- function(x) {
  if (!is.na(x) && x %in% c("TRUE", "FALSE")) {
    x <- as.logical(x)
  }
  return(x)
}
