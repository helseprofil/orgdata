#' @title Delete Selected Rows
#' @description
#' Delete selected rows in the dataset. The rows to be deleted can be specified directly
#' in the Access registration database using minus symbol `-`
#' @inheritParams do_split
#' @inheritParams find_column_input
#' @inheritParams find_spec
#' @import data.table
#' @family delete-raw functions
#' @export
do_delete_row <- function(dt = NULL, spec = NULL, con = NULL) {
  is_debug()
  grp <- spec$FILGRUPPE
  lesid <- spec$LESID
  speCode <- get_delete_row_spec(spec = spec, con = con)

  dt <- is_delete_lesid(dt = dt, code = speCode, lesid = lesid)
  invisible(dt)
}


#' @title Get Rows for Deletion
#' @description Get the raws to be deleted as specified with minus `-` symbol
#' in the codebook based on the `FILGRUPPE` and `LESID` number.
#' Specification group `ALLE` will be used when neither `FILGRUPPE` nor `LESID` is specified.
#' @inheritParams find_column_input
#' @inheritParams find_spec
#' @family delete-raw functions
#' @export
get_delete_row_spec <- function(spec = NULL, con = NULL){
  grp <- spec$FILGRUPPE
  lesid <- spec$LESID
  speCode <- find_spec("delete-raw.sql", con = con, char = grp, num = lesid)
  is_codebook(cb = speCode)
}


## Helper --------------
is_delete_lesid <- function(dt, code, lesid){
  LESID <- KOL <- FRA <- NULL

  idCode <- code[LESID == lesid, list(KOL, FRA)]
  cols <- unique(idCode$KOL)

  dt <- is_delete_row(dt, code = code, cols = cols)
  invisible(dt)
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

  invisible(dt)
}

is_delete_index <- function(dt, delidx){
  # delidx - Row index to be deleted
  keepIdx <- setdiff(dt[, .I], delidx)
  cols = names(dt)
  dtSub <- data.table::data.table(dt[[1]][keepIdx])
  data.table::setnames(dtSub, cols[1])

  for (col in cols[2:length(cols)]){
    dtSub[, (col) := dt[[col]][keepIdx]]
    dt[, (col) := NULL]
  }

  invisible(dtSub)
}
