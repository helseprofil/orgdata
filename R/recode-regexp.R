#' @title Recode Variables with Regular Expression
#' @description Recode variables with regular expression based on the
#'   specification in `tbl_Kode` with TYPE `RE`. The specification in the
#'   codebook can be specific, common or general. \emph{Specific} is when both
#'   `LESID` and `FILGRUPPE` are specified to create a unique reference to be
#'   able to recode the variables. \emph{Common} is when only `FILGRUPPE` is
#'   specified while `LESID` left empty and \emph{general} is when `FILGRUPPE`
#'   is specified with `ALLE` while `LESID` left empty.
#' @inheritParams do_split
#' @inheritParams find_column_input
#' @inheritParams find_spec
#' @import data.table
#' @family recode functions
#' @export
do_recode_regexp <- function(dt = NULL, spec = NULL, con = NULL) {
  is_debug()
  grp <- spec$FILGRUPPE
  lesid <- spec$LESID
  speCode <- get_codebok_regexp(spec = spec, con = con)

  dt <- is_recode_lesid_regexp(dt = dt, code = speCode, lesid = lesid)
  dt <- is_recode_common_regexp(dt = dt, code = speCode, group = grp)
  dt <- is_recode_all_regexp(dt = dt, code = speCode)
  invisible(dt)
}

#' @title Codebook with Regular Expression
#' @description Get the codebook with regular expression to recode variables
#'   based on the `FILGRUPPE` and `LESID` number. Specification group `ALLE`
#'   will be used when neither `FILGRUPPE` nor `LESID` is specified.
#' @inheritParams find_column_input
#' @inheritParams find_spec
#' @family recode functions
#' @export
get_codebok_regexp <- function(spec = NULL, con = NULL){
  grp <- spec$FILGRUPPE
  lesid <- spec$LESID
  speCode <- find_spec("recode-regexp.sql", con = con, char = grp, char2 = lesid)
  is_codebook(cb = speCode)
}

## Helper -----------------------------------------------
## TODO Should combine the code with recode

## When LESID is specified in tbl_Kode
is_recode_lesid_regexp <- function(dt, code, lesid) {
  ## dt - Dataset
  ## code - From codebook
  ## lesid - lesid from file specification
  LESID <- KOL <- FRA <- TIL <- NULL

  idCode <- code[LESID == lesid, list(KOL, FRA, TIL)]
  kols <- unique(idCode$KOL)
  is_recode_regexp(dt = dt, code = idCode, cols = kols)
}

## When LESID in tbl_Kode is empty ie. common within the group
is_recode_common_regexp <- function(dt, code, group) {
  ## dt - Dataset
  ## code - From codebook
  FILGRUPPE <- LESID <- KOL <- FRA <- TIL <- NULL

  allCode <- code[FILGRUPPE == group & is.na(LESID), list(KOL, FRA, TIL)]

  if (nrow(allCode) > 0){
    kols <- unique(allCode$KOL)
    dt <- is_recode_regexp(dt, code = allCode, cols = kols)
  }

  invisible(dt)
}


## When FILGRUPPE in tbl_Kode is ALLE
is_recode_all_regexp <- function(dt, code, aggregate.msg = FALSE){
# aggregate.msg - Either it's KB/RE or AG. Default is KB/RE ie. FALSE

  FILGRUPPE <- KOL <- FRA <- TIL <- NULL

  allCode <- code[FILGRUPPE == "ALLE", list(KOL, FRA, TIL)]

  if (nrow(allCode) > 0) {
    kols <- unique(allCode$KOL)

    if (aggregate.msg){
      msgNotFound <- "Columname(s) defined in codebook for type AGGREGERE (AG) not found:"
      msgFound <- "Columname(s) defined in codebook for type AGGREGERE (AG):"
    } else {
      msgNotFound <- "Columname(s) defined in codebook as ALLE not found:"
      msgFound <- "Columname(s) defined in codebook as ALLE:"
    }

    notCols <- setdiff(kols, names(dt))
    if (length(notCols) > 0){
      is_verbose(paste_cols(notCols), msgNotFound)
    }

    yesCols <- intersect(kols, names(dt))
    if (length(yesCols) > 0){
      is_verbose(paste_cols(yesCols), msgFound)
      dt <- is_recode_regexp(dt = dt, code = allCode, cols = yesCols)
    }
  }

  invisible(dt)
}


## Recode regular exp variable 1-to-1
## Ensure column is string before recode
is_recode_regexp <- function(dt, code, cols){
  i.to <- KOL <- NULL

  for (i in seq_along(cols)){
    col <- cols[i]
    fra <- is_rex(code = code[["FRA"]])
    til <- code[["TIL"]]

    dt[, (col) := gsub(fra, til, get(col))]
  }
  invisible(dt)
}

is_rex <- function(code){
  rexExp <- grepl("^rex\\(", code)
  if (rexExp){
    code <- eval(parse(text = paste0("rex::", code)))
  }

  return(code)
}
