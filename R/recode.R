#' @title Recode Variables
#' @description
#' Recode variables based on the specification in `tbl_Kode` ie. codebook.
#' @inheritParams do_split
#' @inheritParams find_column_input
#' @inheritParams find_spec
#' @import data.table
#' @export
do_recode <- function(dt = NULL, spec = NULL, con = NULL){
  grp <- spec$FILGRUPPE
  lesid <- spec$LESID
  speCode <- find_spec("recode.sql", con=con, character = grp, numeric = lesid)
  data.table::setDT(speCode)

  dt <- is_recode_common(dt = dt, code = speCode)
  is_recode(dt = dt, code = speCode, lesid = lesid)
}


## Helper -----------------------------------------------

## dt - Dataset
## code - From codebook
## lesid - lesid from file specification
is_recode <- function(dt, code, lesid){
  LESID <- KOL <- FRA <- TIL <- NULL
  i.to <- NULL

  idCode <- code[LESID == lesid, list(KOL, FRA, TIL)]
  kols <- unique(idCode$KOL)

  for (i in seq_len(length(kols))){
    col <- kols[i]
    sp <- idCode[KOL == col,]
    dt <- is_NA(dt = dt, code = sp, col = col)
    sp[, KOL := NULL]
    data.table::setnames(sp, names(sp), c(col, "to"))
    dt[sp, on = col, (col) := i.to]
  }
  return(dt)
}

## dt - Dataset
## code - From codebook
is_recode_common <- function(dt, code){
  LESID <- KOL <- FRA <- TIL <- NULL
  i.to <- NULL

  allCode <- code[is.na(LESID), list(KOL, FRA, TIL)]
  kols <- unique(allCode$KOL)

  for (i in seq_len(length(kols))){
    col <- kols[i]
    sp <- allCode[KOL == col,]
    dt <- is_NA(dt = dt, code = sp, col = col)
    sp[, KOL := NULL]
    data.table::setnames(sp, names(sp), c(col, "to"))
    dt[sp, on = col, (col) := i.to]
  }
  return(dt)
}

# dt - Dataset
# code - From codebook
# col - column to recode
is_NA <- function(dt, code, col){
  na <- is.element("NA", code$FRA)
  if (na) {
    dt[is.na(get(col)), (col) := "NA"]
  }
  return(dt)
}
