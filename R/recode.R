#' @title Recode Variables
#' @description
#' Recode variables based on the specification in `tbl_Kode` ie. codebook.
#' `LESID` is the unique reference to recode variables.
#' @inheritParams do_split
#' @inheritParams find_column_input
#' @inheritParams find_spec
#' @import data.table
#' @export
do_recode <- function(dt = NULL, spec = NULL, con = NULL) {
  is_bugs()
  lesid <- spec$LESID
  speCode <- get_codebok(spec = spec, con = con)
  dt <- is_recode_common(dt = dt, code = speCode)
  is_recode(dt = dt, code = speCode, lesid = lesid)
}

#' @title Codebook
#' @description Get the codebook for recoding variables based on the
#'  unique `LESID` number.
#' @inheritParams find_column_input
#' @inheritParams find_spec
#' @export
get_codebok <- function(spec = NULL, con = NULL){
  grp <- spec$FILGRUPPE
  lesid <- spec$LESID
  speCode <- find_spec("recode.sql", con = con, char = grp, num = lesid)
  data.table::setDT(speCode)
}

## Helper -----------------------------------------------
## When LESID is specified in tbl_Kode
is_recode <- function(dt, code, lesid) {
  ## dt - Dataset
  ## code - From codebook
  ## lesid - lesid from file specification
  LESID <- KOL <- FRA <- TIL <- NULL
  i.to <- NULL

  idCode <- code[LESID == lesid, list(KOL, FRA, TIL)]
  kols <- unique(idCode$KOL)

  for (i in seq_len(length(kols))) {
    ## TODO check categories in cols dt is equal to
    ## specification in codebook
    col <- kols[i]
    sp <- idCode[KOL == col, ]
    dt <- is_NA(dt = dt, code = sp, col = col)
    sp[, KOL := NULL]
    data.table::setnames(sp, names(sp), c(col, "to"))
    dt[sp, on = col, (col) := i.to]
  }
  return(dt)
}

## When LESID in tbl_Kode is empty ie. common
is_recode_common <- function(dt, code) {
  ## dt - Dataset
  ## code - From codebook
  LESID <- KOL <- FRA <- TIL <- NULL
  i.to <- NULL

  allCode <- code[is.na(LESID), list(KOL, FRA, TIL)]
  kols <- unique(allCode$KOL)

  for (i in seq_len(length(kols))) {
    ## TODO check categories in cols dt is equal to
    ## specification in codebook
    col <- kols[i]
    sp <- allCode[KOL == col, ]
    dt <- is_NA(dt = dt, code = sp, col = col)
    sp[, KOL := NULL]
    data.table::setnames(sp, names(sp), c(col, "to"))
    dt[sp, on = col, (col) := i.to]
  }
  return(dt)
}

## For easy converstion to find NA as string
is_NA <- function(dt, code, col) {
  ## dt - Dataset
  ## code - From codebook
  ## col - column to recode
  na <- is.element("NA", code$FRA)
  if (na) {
    dt[is.na(get(col)), (col) := "NA"]
  }
  return(dt)
}
