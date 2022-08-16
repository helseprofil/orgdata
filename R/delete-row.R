#' @title Delete Selected Rows
#' @description
#' Delete selected rows in the dataset. The rows to be deleted can be specified directly
#' in the Access registration database using minus symbol `-`
#' @inheritParams do_split
#' @inheritParams find_column_input
#' @inheritParams find_spec
#' @import data.table
#' @family delete-row functions
#' @export
do_delete_row <- function(dt = NULL, spec = NULL, con = NULL) {
  is_debug()
  grp <- spec$FILGRUPPE
  lesid <- spec$LESID
  speCode <- get_delete_row_spec(spec = spec, con = con)

  dt <- is_delete_lesid(dt = dt, code = speCode, lesid = lesid)
  dt <- is_delete_common(dt = dt, code = speCode, group = grp)
  dt <- is_delete_all(dt = dt, code = speCode)
}


#' @title Get Rows for Deletion
#' @description Get the rows to be deleted as specified with minus `-` symbol
#' in the codebook based on the `FILGRUPPE` and `LESID` number.
#' Specification group `ALLE` will be used when neither `FILGRUPPE` nor `LESID` is specified.
#' @inheritParams find_column_input
#' @inheritParams find_spec
#' @family delete-row functions
#' @export
get_delete_row_spec <- function(spec = NULL, con = NULL){
  grp <- spec$FILGRUPPE
  lesid <- spec$LESID
  speCode <- find_spec("delete-row.sql", con = con, char = grp, char2 = lesid)
  is_codebook(cb = speCode)
}


## Helper --------------
is_delete_lesid <- function(dt, code, lesid){
  LESID <- KOL <- FRA <- NULL

  idCode <- code[LESID == lesid, list(KOL, FRA)]
  cols <- unique(idCode$KOL)
  dt <- is_delete_row(dt, code = idCode, cols = cols)
}


is_delete_common <- function(dt, code, group){
  FILGRUPPE <- LESID <- KOL <- FRA <- TIL <- NULL

  allCode <- code[FILGRUPPE == group & is.na(LESID), list(KOL, FRA)]
  cols <- unique(allCode$KOL)
  dt <- is_delete_row(dt, code = allCode, cols = cols)
  return(dt)
}

is_delete_all <- function(dt, code){
  FILGRUPPE <- KOL <- FRA <- TIL <- NULL

  allCode <- code[FILGRUPPE == "ALLE", list(KOL, FRA)]
  cols <- unique(allCode$KOL)

  notCols <- setdiff(cols, names(dt))
  if (length(notCols) > 0){
    is_verbose(paste_cols(notCols), "Columname(s) defined in ALLE for row deleting not found:", type = "warn")
  }

  yesCols <- intersect(cols, names(dt))
  if (length(yesCols) > 0){
    is_verbose(paste_cols(yesCols), "Columname(s) defined in ALLE for row deleting:")
    dt <- is_delete_row(dt = dt, code = allCode, cols = yesCols)
  }

  return(dt)
}


is_delete_row <- function(dt, code, cols){
  KOL <- FRA <- NULL

  for (i in seq_along(cols)){
    col <- cols[i]
    del <- code[KOL == col, FRA]
    dt <- is_NA(dt = dt, code = code[KOL == col], col = col)
    delidx <- dt[get(col) == del, which = TRUE]
    dt <- is_delete_index(dt, delidx)
  }

  return(dt)
}

## Ref https://github.com/Rdatatable/data.table/issues/635#issuecomment-261473829
is_delete_index <- function(dt, delidx){
  # delidx - Row index to be deleted
  keepIdx <- setdiff(dt[, .I], delidx)
  cols = names(dt)
  dtSub <- data.table::data.table(dt[[1]][keepIdx]) #subsetted table
  data.table::setnames(dtSub, cols[1])

  for (col in cols[2:length(cols)]){
    dtSub[, (col) := dt[[col]][keepIdx]]
    dt[, (col) := NULL]
  }

  return(dtSub)
}
