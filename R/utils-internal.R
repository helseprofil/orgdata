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
    is_stop(msgTxt)
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
    is_stop(msgTxt)
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
    is_stop(msgTxt)
  }
}

## TABS and VALS should be extansible
is_standard_cols <- function(){
  landsplit <- c("LANDBAK", "INNVKAT")
  c(getOption("orgdata.columns"),
    landsplit,
    paste0("TAB", 1:getOption("orgdata.tabs")),
    paste0("VAL", 1:getOption("orgdata.vals")))
}

# Standard dummy input starts with symbol $
# eg. column AAR with $Y
is_col_dummy <- function(col) {
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

# When NA from Access is a character and not real NA
is_fake_NA <- function(x){
  isNA <- c("<NA>", "NA")
  fake <- is.element(x, isNA)

  if (fake){
    x <- NA
  }

  return(x)
}


is_verbose <- function(x = NULL, msg = NULL,
                       type = c("note", "warn", "warn2",
                                "error", "error2", "other", "debug"),
                       ctrl = parent.frame(),
                       sign = FALSE) {
  ## x - Arg or object to show in the message
  type <- match.arg(type)

  if (!is.null(msg)) {
    msg <- msg
  } else {
    msg <- ""
  }

  if (is.environment(ctrl)){
    control <- ctrl[["control"]]
  } else {
    control <- ctrl
  }

  if(is.null(control)) control <- FALSE

  if (control){
    sign <- TRUE
    x <- ""
    msg <- "File is clean"
    type <- "debug"
  }

  if (getOption("orgdata.verbose")) {
    is_colour_txt(x = x, msg = msg, type = type, sign = sign)
  }
}

is_debug <- function() {
  if (getOption("orgdata.debug")) {
    fnc <- sys.calls()[[sys.nframe() - 1]][1]
    is_colour_txt(x = deparse(fnc), msg = "Execute:", type = "debug")
    ## cat(note(paste0("Execute:  ", deparse(fnc))))
  }
}

is_debug_warn <- function(opt){
  txt <- "Data wrangling discontinued! Debugging on:"
  is_color_txt(x = opt, msg = txt, type = "warn2")
}


is_stop <- function(msg, var = NULL){
  if (!is.null(var)){
    var <- is_long_vector(var)
    msg <- paste(msg, var, sep = " ")
  }
  stop(simpleError(msg))
}

is_long_vector <- function(vec){
  if (length(vec) > 1){
    vec <- paste_cols(vec)
  }
  return(vec)
}

## For paste of long vectors ie. many columnames,
## to be nicely displayed in a message
paste_cols <- function(cols){
  paste0('"', paste(cols, collapse = '", "'), '"')
}

## Default orgdata path is used to save file when not using filegroup
## or log files
is_orgdata_path <- function(dir = c("home", "temp")){

  dir <- match.arg(dir)
  userPath <- switch(dir,
                     home = fs::path_home(),
                     temp = fs::path_temp())

  orgPath <- file.path(userPath, "orgdata_files")

  if (!fs::dir_exists(orgPath)){
    is_verbose(x = orgPath, msg = "Use default folder:")
    fs::dir_create(orgPath)
  }

  invisible(orgPath)
}

## Display message with selected colours
is_colour_txt <- function(x, msg,
                          type = c("note", "warn", "warn2",
                                   "error", "error2", "other", "debug"),
                          sign = FALSE){
  ## msg - Message to display
  ## x - Object to display in the message
  type <- match.arg(arg = type)

  txtRed <- crayon::make_style("red")
  txtMagenta <- crayon::make_style("magenta")
  txtBold <- crayon::make_style("bold")

  noteClr <- crayon::green
  warnClr <- crayon::magenta
  warn2Clr <- crayon::combine_styles(txtRed, txtBold)
  errorClr <- crayon::red
  otherClr <- crayon::blue
  debugClr <- crayon::silver

  clrMsg <- switch(type,
                   note = noteClr(paste0(msg, " ", crayon::blue(x))),
                   warn = warnClr(paste0("Warning: ", msg, " ", x)),
                   warn2 = warn2Clr(paste0(msg, " ", crayon::blue(x))),
                   error = errorClr(paste0(msg, " ", x)),
                   error2 = errorClr(paste0(msg, " ", crayon::green(x))),
                   other = otherClr(paste0(msg)),
                   debug = debugClr(paste0(msg, " ", crayon::green(x))))

  symb <- switch(getOption("orgdata.emoji"),
                 thumb = "\U0001F44D",
                 mark = "\U0002713",
                 smile = "\U0001F60A",
                 sad = "\U002639",
                 santa = "\U0001F385"
                 )

  if (sign){
    cat(symb, clrMsg, "\n")
  } else {
    cat(clrMsg, "\n")
  }
}

is_color_txt <- is_colour_txt

is_line_long <- function(){
  paste0("==============================================================")
}

is_line_short <- function(){
  paste0("------------------------------------------")
}

is_batch <- function(type = c("date", "time")){
  type <- match.arg(type)
  switch(type,
         time = format(Sys.time(), format = "%Y%m%d_%H%M%S"),
         date = Sys.Date(),
         Sys.Date()
         )
}

