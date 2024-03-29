#' @title Recode Variables
#' @description
#' Recode variables based on the specification in `tbl_KodeBok` ie. codebook.
#' `LESID` must be combined with `FILGRUPPE` to create a unique reference to
#' be able to recode the variables. Specification group `ALLE` will be
#' used when neither `FILGRUPPE` nor `LESID` is specified.
#' @inheritParams do_split
#' @inheritParams find_column_input
#' @inheritParams find_spec
#' @inheritParams do_geo_recode
#' @import data.table
#' @family recode functions
#' @export
do_recode <- function(dt = NULL, spec = NULL, con = NULL, control = FALSE) {
  is_debug()
  grp <- spec$FILGRUPPE
  lesid <- spec$LESID
  speCode <- get_codebok(spec = spec, con = con)
  dt <- is_recode_lesid(dt = dt, code = speCode, lesid = lesid)
  dt <- is_recode_common(dt = dt, code = speCode, group = grp)
  dt <- is_recode_all(dt = dt, code = speCode, control = control)
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
  speCode <- find_spec("recode.sql", con = con, char = grp, char2 = lesid)
  is_codebook(cb = speCode)
}

## Helper -----------------------------------------------
## When LESID is specified in tbl_KodeBok
is_recode_lesid <- function(dt, code, lesid) {
  ## dt - Dataset
  ## code - From codebook
  ## lesid - lesid from file specification
  LESID <- KOL <- FRA <- TIL <- NULL

  idCode <- code[LESID == lesid, list(KOL, FRA, TIL)]
  check_dublicate_col(idCode)

  kols <- unique(idCode$KOL)
  is_recode(dt = dt, code = idCode, cols = kols)
}

## When LESID in tbl_KodeBok is empty ie. common within the group
is_recode_common <- function(dt, code, group) {
  ## dt - Dataset
  ## code - From codebook
  FILGRUPPE <- LESID <- KOL <- FRA <- TIL <- NULL

  allCode <- code[FILGRUPPE == group & is.na(LESID), list(KOL, FRA, TIL)]

  if (nrow(allCode) > 0){
    ## check_dublicate_col(allCode)
    kols <- unique(allCode$KOL)
    dt <- is_recode(dt, code = allCode, cols = kols)
  }

  return(dt)
}


## When FILGRUPPE in tbl_KodeBok is ALLE
is_recode_all <- function(dt, code, aggregate.msg = FALSE, control = FALSE){
  # aggregate.msg - If it's KB or AG

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
      dt <- is_recode(dt = dt, code = allCode, cols = yesCols)
    }
  }

  return(dt)
}


## Recode variable 1-to-1
## Ensure column is string before recode
is_recode <- function(dt, code, cols){
  i.to <- KOL <- NULL

  for (i in seq_along(cols)){
    col <- cols[i]
    sp <- code[KOL == col,]
    dt <- is_NA(dt = dt, code = sp, col = col)
    sp[, KOL := NULL]
    data.table::setnames(sp, names(sp), c(col, "to"))
    dt <- is_column_char(dt, col)
    dt[sp, on = col, (col) := i.to]
  }
  return(dt)
}

## For easy converstion to find NA as string
is_NA <- function(dt, code, col) {
  ## dt - Dataset
  ## code - From codebook
  ## col - column to recode
  isNA <- c("<NA>", "NA")
  chrNA <- intersect(isNA, code$FRA)

  na <- length(chrNA) > 0
  if (na) {
    dt <- is_column_char(dt, col)
    dt[is.na(get(col)), (col) := chrNA]
  }
  return(dt)
}

## To ensure recode works all the time especially
## for aggregate (AG) with imposed "NA"
is_column_char <- function(dt, col){
  chrCol <- is(dt[[col]], "character")
  if (isFALSE(chrCol)){
    dt[, (col) := as.character(get(col))]
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


check_dublicate_col <- function(code){

  dp <- data.table::copy(code)
  cols <- c("KOL", "FRA")
  dp[, "dup" := .N > 1, by = cols]
  dps <- sum(dp[["dup"]])

  if (dps > 0){
    is_stop("Recode variable is not unique for", unique(code[["KOL"]]))
  }
  invisible()
}
