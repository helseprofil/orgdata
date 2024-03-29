# Check code ------------------------------------------
# Arguments that are missing
is_null <- function(arg = NULL, msg = NULL, verbose = NULL) {
  ## msg : Alternative message if not using default
  argchr <- deparse(substitute(arg))

  if (is.null(verbose)) verbose <- getOption("orgdata.verbose")

  if (!is.null(msg)) {
    msgTxt <- msg
  } else {
    msgTxt <- sprintf("Argument for `%s` is missing!", argchr)
  }

  if (verbose) {
    fun <- deparse(sys.calls()[[sys.nframe() - 1]])
    if (length(fun) > 1) fun <- paste0(fun, collapse = "")
    msgTxt <- paste0("\n", msgTxt, " in \n", fun)
  }

  if (is.null(arg)) {
    is_stop(msgTxt)
  }
}

is_null_both <- function(x = NULL, y = NULL, msg = NULL, verbose = NULL) {
  xchr <- deparse(substitute(x))
  ychr <- deparse(substitute(y))

  if (is.null(verbose)) verbose <- getOption("orgdata.verbose")

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

is_not_null_both <- function(x = NULL, y = NULL, msg = NULL, verbose = NULL) {
  xchr <- deparse(substitute(x))
  ychr <- deparse(substitute(y))

  if (is.null(verbose)) verbose <- getOption("orgdata.verbose")

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

## TABS and VALS should be extensible
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
is_separate <- function(x, sep = NULL, keep = NULL, fixed = TRUE) {
  # x : the string to be separated
  # sep : separate symbole like ",","=" etc.
  # keep : Keep 1=lhs or 2=rhs eg. x[1] for lhs
  is_null(sep)
  if (methods::is(x, "numeric")) {
    x <- as.character(x)
  }
  out <- unlist(strsplit(x, sep, fixed = fixed))
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
                       emoji = FALSE) {
  ## x - Arg or object to show in the message
  ## ctrl - If KONTROLLERT is marked then don't show message
  type <- match.arg(type)

  if (!is.null(msg)) {
    msg <- msg
  } else {
    msg <- ""
  }

  # Use parant.frame to find object "control"
  if (is.environment(ctrl)){
    control <- ctrl[["control"]]
  } else {
    control <- ctrl
  }

  if(is.null(control)) control <- FALSE

  if (control){
    emoji <- TRUE
    x <- "see_data()"
    msg <- "File has been checked or added to warehouse. Read with"
    type <- "debug"
  }

  if (getOption("orgdata.verbose")) {
    is_colour_txt(x = x, msg = msg, type = type, emoji = emoji)
  }
}

## Show helper functions with "deep"
is_debug <- function(type = NULL, deep = FALSE, showFUN = FALSE) {

  if (is.null(type)) type <- getOption("orgdata.debug")

  if (isFALSE(type)){
    return()
  }

  if (isTRUE(type) || type == "shallow") showFUN <- TRUE

  if (isTRUE(deep)) showFUN <- FALSE
  if (type == "deep"){
    showFUN <- TRUE
  }

  if (showFUN) {
    fnc <- sys.calls()[[sys.nframe() - 1]][1]
    is_colour_txt(x = deparse(fnc), msg = "Execute:", type = "debug")
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

  orgPath <- file.path(userPath, "orgdata_logs")

  if (!fs::dir_exists(orgPath)){
    is_verbose(x = orgPath, msg = "Create folder for log:")
    fs::dir_create(orgPath)
  }

  invisible(orgPath)
}

## Display message with selected colours
is_colour_txt <- function(x, msg,
                          type = c("note", "warn", "warn2",
                                   "error", "error2", "other", "debug"),
                          emoji = FALSE,
                          symbol = NULL){
  ## msg - Message to display
  ## x - Object to display in the message
  ## emoji - Add emoji

  if (is.null(symbol))
    symbol <- getOption("orgdata.emoji")

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

  symb <- switch(symbol,
                 thumb = "\U0001F44D",
                 write = "\U00270D",
                 mark = "\U0002713",
                 smile = "\U0001F60A",
                 sad = "\U001F629",
                 santa = "\U0001F385",
                 search = "\U001F50D",
                 folder = "\U001F4C1",
                 paper = "\U001F4F0",
                 book = "\U0001F4DA",
                 safe = "\U001F4E6"
                 )

  if (emoji){
    cat(symb, clrMsg, "\n")
  } else {
    cat(clrMsg, "\n")
  }
}

is_party <- function(){
  emo <- c("\U001F389", "\U001F973", "\U001F37A", "\U001F483", "\U001F37E", "\U001F938",
           "\U0001F60A", "\U001F378", "\U001F379","\U002728", "\U001F942", "\U001F57A",
           "\U001F377", "\U001F943", "\U001F601", "\U001F929", "\U001F60E")
  sample(emo, 1)
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

is_package_condition <- function(pkg, arg){
  # pkgs must be as list if multiples

  yesno <- sapply(pkg, function(x) requireNamespace(x, quietly = TRUE))
  pkgs <- unlist(pkg[yesno])
  nopkg <- setdiff(unlist(pkg), pkgs)

  if (length(nopkg) > 0){
    pp <- is_long_vector(nopkg)
    msg <- sprintf("Please install %s package(s) to use arg %s", pp, arg)
    is_stop(msg)
  }

  invisible()
}

# end - ending of each word
is_word <- function(end = NULL){
  words <- c("job", "effort", "workflow", "endeavor",
             "accomplishment", "achievement", "work",
             "precision", "attainment", "production",
             "and you nailed it")

  if (!is.null(end))
    word <- paste0(words, end)

  sample(word, 1)
}
