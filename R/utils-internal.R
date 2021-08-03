# Check code ------------------------------------------
# Arguments that are missing
is_null <- function(arg = NULL, msg = NULL) {
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

is_null_also <- function(x = NULL, y = NULL, msg = NULL) {
  xchr <- deparse(substitute(x))
  ychr <- deparse(substitute(y))

  if (!is.null(msg)) {
    msgTxt <- msg
  } else {
    msgTxt <- sprintf("Either `%s` or `%s` can't be empty", xchr, ychr)
  }

  if (is.null(x) && is.null(y)) {
    stop(msgTxt)
  }
}

is_null_both <- function(x = NULL, y = NULL, msg = NULL) {
  xchr <- deparse(substitute(x))
  ychr <- deparse(substitute(y))

  if (!is.null(msg)) {
    msgTxt <- msg
  } else {
    msgTxt <- sprintf("Only one of `%s` or `%s` can be used", xchr, ychr)
  }

  if (!is.null(x) && !is.null(y)) {
    stop(msgTxt)
  }
}



# Standard dummy input starts with symbol $
# eg. column AAR with $Y
is_dummy <- function(x) {
  grepl("^\\$", x)
}

# To separate a string with selected sep symbol
# Can keep both value ie. lhs and rhs or either one
is_separate <- function(x, sep = NULL, keep = NULL) {
  # x : the string to be separated
  # sep : separate symbole like ",","=" etc.
  # keep : Keep lhs or rhs eg. x[1] for lhs
  is_null(sep)
  out <- unlist(strsplit(x, sep))
  if (!is.null(keep)) {
    out <- out[keep]
  }

  trimws(out)
}

# TRUE/FALSE is character when fetching data from DB
is_logical <- function(x) {
  if (!is.na(x) && x %in% c("TRUE", "FALSE")) {
    x <- as.logical(x)
  }
  return(x)
}
