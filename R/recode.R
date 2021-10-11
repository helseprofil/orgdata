#' @title Recode Variables
#' @description
#' Recode variables based on the specification in `tbl_Kode` ie. codebook.
#' `LESID` must be combined with `FILGRUPPE` to create a unique reference to
#' be able to recode the variables. Specification group `ALLE` will be
#' used when neither `FILGRUPPE` nor `LESID` is specified.
#' @inheritParams do_split
#' @inheritParams find_column_input
#' @inheritParams find_spec
#' @import data.table
#' @family recode functions
#' @export
do_recode <- function(dt = NULL, spec = NULL, con = NULL) {
  is_debug()
  grp <- spec$FILGRUPPE
  lesid <- spec$LESID
  speCode <- get_codebok(spec = spec, con = con)
  dt <- is_recode_lesid(dt = dt, code = speCode, lesid = lesid)
  dt <- is_recode_common(dt = dt, code = speCode, group = grp)
  dt <- is_recode_all(dt = dt, code = speCode)
  invisible(dt)
}

#' @title Codebook
#' @description Get the codebook for recoding variables based on the
#'  `FILGRUPPE` and `LESID` number. Specification group `ALLE` will be
#'  used when neither `FILGRUPPE` nor `LESID` is specified.
#' @inheritParams find_column_input
#' @inheritParams find_spec
#' @family recode functions
#' @export
get_codebok <- function(spec = NULL, con = NULL){
  grp <- spec$FILGRUPPE
  lesid <- spec$LESID
  speCode <- find_spec("recode.sql", con = con, char = grp, num = lesid)
  is_codebook(cb = speCode)
}

## Helper -----------------------------------------------
## When LESID is specified in tbl_Kode
is_recode_lesid <- function(dt, code, lesid) {
  ## dt - Dataset
  ## code - From codebook
  ## lesid - lesid from file specification
  LESID <- KOL <- FRA <- TIL <- NULL

  idCode <- code[LESID == lesid, list(KOL, FRA, TIL)]
  kols <- unique(idCode$KOL)
  is_recode(dt = dt, code = idCode, cols = kols)
}

## When LESID in tbl_Kode is empty ie. common within the group
is_recode_common <- function(dt, code, group) {
  ## dt - Dataset
  ## code - From codebook
  FILGRUPPE <- LESID <- KOL <- FRA <- TIL <- NULL

  allCode <- code[FILGRUPPE == group & is.na(LESID), list(KOL, FRA, TIL)]

  if (nrow(allCode) > 0){
    kols <- unique(allCode$KOL)
    dt <- is_recode(dt, code = allCode, cols = kols)
  }

  invisible(dt)
}


## When FILGRUPPE in tbl_Kode is ALLE
is_recode_all <- function(dt, code, aggregate.msg = FALSE){
  FILGRUPPE <- KOL <- FRA <- TIL <- NULL

  allCode <- code[FILGRUPPE == "ALLE", list(KOL, FRA, TIL)]

  if (nrow(allCode) > 0) {
    kols <- unique(allCode$KOL)

    if (aggregate.msg){
      msgNotFound <- "Columname(s) defined in AGGREGERE for recoding not found:"
      msgFound <- "Columname(s) defined in AGGREGERE for recoding:"
    } else {
      msgNotFound <- "Columname(s) defined in ALLE for recoding not found:"
      msgFound <- "Columname(s) defined in ALLE for recoding:"
    }

    notCols <- setdiff(kols, names(dt))
    if (length(notCols) > 0){
      is_verbose(paste_cols(notCols), msgNotFound)
    }

    yesCols <- intersect(kols, names(dt))
    if (length(yesCols) > 0){
      is_verbose(paste_cols(yesCols), msgFound)
      dt <- is_recode(dt = dt, code = allCode, cols = yesCols)
    }
  }

  invisible(dt)
}


## Recode variable 1-to-1
is_recode <- function(dt, code, cols){
  i.to <- KOL <- NULL

  for (i in seq_along(cols)){
    col <- cols[i]
    sp <- code[KOL == col,]
    dt <- is_NA(dt = dt, code = sp, col = col)
    sp[, KOL := NULL]
    data.table::setnames(sp, names(sp), c(col, "to"))
    dt[sp, on = col, (col) := i.to]
  }
  invisible(dt)
}

## For easy converstion to find NA as string
is_NA <- function(dt, code, col) {
  ## dt - Dataset
  ## code - From codebook
  ## col - column to recode
  isNA <- c("<NA>", "NA")
  naIdx <- is.element(isNA, code$FRA)
  chrNA <- isNA[naIdx]

  na <- sum(naIdx) > 0
  if (na) {
    dt[is.na(get(col)), (col) := chrNA]
  }
  return(dt)
}

## When has LESID needs FILGRUPPE too because
## LESID is not unique
is_codebook <- function(cb){
  LESID <- FILGRUPPE <- NULL

  data.table::setDT(cb)
  cbErr <- cb[!is.na(LESID) & is.na(FILGRUPPE),]

  if (nrow(cbErr) > 0){
    is_stop("FILGRUPPE is not found for LESID:", cbErr$LESID)
  }

  invisible(cb)
}
