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
is_dummy <- function(col) {
  expMutate <- "^<.*>$"
  expYear <- "^\\$"
  expr <- c(expMutate, expYear)
  dummy <- sapply(expr, function(x) grepl(x, col))
  sum(dummy) != 0
}


# To separate a string with selected sep symbol
# Can keep both value ie. lhs and rhs or either one
is_separate <- function(x, sep = NULL, keep = NULL) {
  # x : the string to be separated
  # sep : separate symbole like ",","=" etc.
  # keep : Keep 1=lhs or 2=rhs eg. x[1] for lhs
  is_null(sep)
  if (methods::is(x, "numeric")) {
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



is_verbose <- function(x = NULL, msg = NULL,
                       type = c("note", "warn", "warn2",
                                "error", "other", "debug")) {
  ## x - Arg or object to show in the message
  type <- match.arg(type)

  if (!is.null(msg)) {
    msg <- msg
  } else {
    msg <- ""
  }

  if (getOption("orgdata.verbose")) {
    is_colour(x = x, msg = msg, type = type)
  }
}


is_debug <- function() {
  if (getOption("orgdata.debug")) {
    fnc <- sys.calls()[[sys.nframe() - 1]][1]
    is_colour(x = deparse(fnc), msg = "Execute:", type = "debug")
    ## cat(note(paste0("Execute:  ", deparse(fnc))))
  }
}


is_stop <- function(msg, var = NULL){
  if (!is.null(var)){
    msg <- paste(msg, var, sep = " ")
  }
  stop(simpleError(msg))
}


## For paste of long vectors ie. many columnames,
## to be nicely displayed in a message
paste_cols <- function(cols){
  paste0('"', paste(cols, collapse = '", "'), '"')
}

## Display message with selected colours
is_colour <- function(x, msg, type = c("note", "warn", "warn2",
                                       "error", "other", "debug")){
  ## msg - Message to display
  ## x - Object to display in the message

  txtClr <- crayon::make_style("magenta")
  txtBld <- crayon::make_style("bold")

  noteClr <- crayon::green
  warnClr <- crayon::magenta
  warn2Clr <- crayon::combine_styles(txtClr, txtBld)
  errorClr <- crayon::red
  otherClr <- crayon::blue
  debugClr <- crayon::silver

  switch(type,
         note = cat(noteClr(paste0(msg, " ", crayon::blue(x), "\n"))),
         warn = cat(warnClr(paste0("Warning: ", msg, " ", x, "\n"))),
         warn2 = cat(warn2Clr(paste0(msg, " ", crayon::blue(x), "\n"))),
         error = cat(errorClr(paste0(msg, " ", x, "\n"))),
         other = cat(otherClr(paste0(msg, "\n"))),
         debug = cat(debugClr(paste0(msg, " ", crayon::green(x), "\n"))))

}
