# Check code ------------------------------------------
# Arguments that are missing
is_null <- function(arg = NULL, msg = NULL, verbose = getOption("orgdata.verbose")) {
  ## msg : Alternative message if not using default
  argchr <- deparse(substitute(arg))

  if (!is.null(msg)) {
    msgTxt <- msg
  } else {
    msgTxt <- sprintf("Argument for `%s` is missing", argchr)
  }

  if (verbose) {
    fun <- deparse(sys.calls()[[sys.nframe() - 1]])
    msgTxt <- paste0("\n", msgTxt, " in ", fun)
  }

  if (is.null(arg)) {
    stop(msgTxt)
  }
}

is_null_both <- function(x = NULL, y = NULL, msg = NULL, verbose = getOption("orgdata.verbose")) {
  xchr <- deparse(substitute(x))
  ychr <- deparse(substitute(y))

  if (!is.null(msg)) {
    msgTxt <- msg
  } else {
    msgTxt <- sprintf("Either `%s` or `%s` can't be empty", xchr, ychr)
  }

  if (verbose) {
    fun <- deparse(sys.calls()[[sys.nframe() - 1]])
    msgTxt <- paste0("\n", msgTxt, " in ", fun)
  }

  if (is.null(x) && is.null(y)) {
    stop(msgTxt)
  }
}

is_not_null_both <- function(x = NULL, y = NULL, msg = NULL, verbose = getOption("orgdata.verbose")) {
  xchr <- deparse(substitute(x))
  ychr <- deparse(substitute(y))

  if (!is.null(msg)) {
    msgTxt <- msg
  } else {
    msgTxt <- sprintf("Either `%s` or `%s` can be used and not both", xchr, ychr)
  }

  if (verbose) {
    fun <- deparse(sys.calls()[[sys.nframe() - 1]])
    msgTxt <- paste0("\n", msgTxt, " in ", fun)
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
  if (class(x) == "numeric") {
    x <- as.character(x)
  }
  out <- unlist(strsplit(x, sep, fixed = TRUE))
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


is_verbose <- function(x = NULL, msg = NULL, type = c("message", "warning")) {
  type <- match.arg(type)

  if (!is.null(msg)) {
    msg <- msg
  } else {
    msg <- ""
  }

  if (getOption("orgdata.verbose")) {
    switch(type,
           message = message(msg, " ", x),
           warning = warning(msg, " ", x)
           )
  }
}

is_debug <- function() {
  if (getOption("orgdata.debug")) {
    print(sys.calls()[[sys.nframe() - 1]])
  }
}


is_stop <- function(msg, var = NULL){
  if (!is.null(var)){
    msg <- paste(msg, var, sep = " ")
  }
  stop(simpleError(msg))
}
