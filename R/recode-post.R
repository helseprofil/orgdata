#' @title Post Recode Columns
#' @description Recode selected columns after cleaning and processing the
#'   filegroup based on the specification for `FILGRUPPE` in codebook with type
#'   `PS`. For example some columns in a filegroup doesn't have values for a
#'   specific year. This column should not have a value of 0. Instead a special
#'   symbol `..` is given, representing that the column is missing because the
#'   value is not available to calculate the value.
#' @inheritParams do_split
#' @inheritParams get_split
#' @inheritParams find_spec
#' @inheritParams do_geo_recode
#' @family recode functions
#' @export
do_recode_post <- function(dt = NULL,
                           spec = NULL,
                           con = NULL,
                           control = FALSE) {
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
#TODO Delete raw with "-"

is_recode_post <- function(dt, ...){

  vars <- list(...)
  spec <- vars[["spec"]]
  recodeCol <- vars[["recodeCol"]]
  toVAL <- vars[["toVAL"]]
  input <- vars[["input"]] #recode spec

  # Ensure similar type for column and value to be changed to
  val <- as.numeric(toVAL)
  if (is.na(val)){
    dt[, (recodeCol) := as.character(get(recodeCol))]
  }

  ## Either RAW or EXP input
  typ <- is_post_type(input)

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
  argInput <- sub("^raw\\((.*)\\)", "\\1", input)
  argVAL <- sub("^\\((.*)\\)", "\\1", argInput)
  str2lang(argVAL)
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
