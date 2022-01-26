#' @title Post Recode Columns
#' @description Recode selected columns after cleaning and processing the
#'   filegroup based on the specification for `FILGRUPPE` in codebook with type
#'   `PS`. For example some columns in a filegroup doesn't have values for a
#'   specific year. This column should not have a value of 0. Instead a special
#'   symbol `..` is given, representing that the column is missing because the
#'   value is not available for calculation. This is how to specify in `FRA`
#'   column in registration database to recode all years from 1990 to 2000 and
#'   TAB1 with value `foo` to a selected value in `KOL` column ie. `AAR =
#'   1990:2000 & TAB1 = "foo"`. Important to use symbol `&` if more than one
#'   conditions to recode. To use R code syntax directly asis, use `raw` prefix
#'   ie. `raw(AAR == 1990:2000 & TAB1 == "foo")`. Selection with `%in%`,
#'   `%chin%`, `|` etc. can be used with `raw` prefix.
#' @inheritParams do_split
#' @inheritParams get_split
#' @inheritParams find_spec
#' @family recode functions
#' @export
do_recode_post <- function(dt = NULL,
                           spec = NULL,
                           con = NULL) {
  is_debug()

  grp <- find_column_input(spec = spec, "FILGRUPPE")
  speCode <- find_spec("recode-post.sql", con = con, value = grp)

  for (i in seq_len(nrow(speCode))){
    rpost <- speCode[i,]
    recodeCol <- find_column_input(spec = rpost, col = "KOL")
    toVAL <- find_column_input(spec = rpost, col = "TIL")
    fromVAL <- find_column_input(spec = rpost, col = "FRA")

    dt <- is_recode_post(dt = dt,
                         spec = rpost,
                         input = fromVAL,
                         recodeCol = recodeCol,
                         toVAL = toVAL)
  }

  return(dt)
}

### Helper -------------------
# TODO Use S3 class
is_recode_post <- function(dt, ...){

  is_debug(deep = TRUE)

  vars <- list(...)
  spec <- vars[["spec"]] #Codebook spec
  recodeCol <- vars[["recodeCol"]] #column to recode
  toVAL <- vars[["toVAL"]] #value to be recode to
  input <- vars[["input"]] #i arg or condition to recode

  # Ensure similar type for column and value to be changed to
  val <- suppressWarnings(as.numeric(toVAL))
  if (is.na(val)){
    dt[, (recodeCol) := as.character(get(recodeCol))]
  }

  ## Either RAW or EXP input
  typ <- is_post_type(input)

  dt <- is_post_delete_row(dt, spec, input, toVAL, typ)

  if (typ == "raw"){
    argOut <- is_post_raw(input = input)
    dt[eval(argOut), (recodeCol) := toVAL]
  } else {
    argOut <- is_post_exp(input = spec)
    cols <- names(argOut)
    dt[argOut, (recodeCol) := toVAL, on = cols]
  }
  return(dt)
}

## Raw input and evaluate asis
is_post_raw <- function(input){

  ## Check if the parentheses are balanced
  lhs <- length(gregexpr("\\(", input)[[1]])
  rhs <- length(gregexpr("\\)", input)[[1]])
  para <- lhs - rhs

  if (para < 0){
    is_stop(msg = "Parentheses in `Post Recode` are not balanced!", var = input)
  }

  if (para == 0){
    argInput <- sub("^raw\\((.*)\\)", "\\1", input)
  } else {
    argInput <- sub("^raw\\((.*)", "\\1", input)
  }

  str2lang(argInput)
}



## Expression not an R standard
is_post_exp <- function(input){
  argInput <- find_column_multi(input, "FRA", sep = "&")
  argVAL <- find_column_multi_input(argInput)

  numCols <- intersect(getOption("orgdata.num"), names(argVAL))

  if (length(numCols) > 0){
    for (i in numCols){
      tryCatch(
        as.numeric(argVAL[[i]]),
        warning = function(er) {
          is_stop(msg = "Post recode codebook `FRA` is not numeric for", var = i)
        }
      )

      argVAL[[i]] <- eval(str2lang(argVAL[[i]]))
    }
  }

  return(argVAL)
}

is_post_type <- function(input){
  if (grepl("^raw", input)){
    out <- "raw"
  } else {
    out <- "exp"
  }
}

# Delete row
is_post_delete_row <- function(dt, spec, input, toVAL, typ){

  if (toVAL != "-"){
    return(dt)
  }

  if (typ == "raw"){
    argOut <- is_post_raw(input = input)
    idx <- dt[eval(argOut), which = TRUE]
  } else {
    argOut <- is_post_exp(input = spec)
    cols <- names(argOut)
    idx <- dt[argOut, which = TRUE, on = cols]
  }

  is_delete_index(dt, idx)
}
