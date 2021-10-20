#' @title Split Column
#' @description Split the columns as specified in the registration database.
#' @param dt Input data preferrably in a \code{data.table} format but is not mandatory
#' @param split Split specification as a list. Should be equivalent
#'    to the output of [get_split()] function
#' @importFrom methods is
#' @export
do_split <- function(dt = NULL, split = NULL) {

  if (is.na(split$from)){
    return(dt)
  }

  is_debug()
  is_null(dt, "Data set not found!")
  is_null(split)

  if (isFALSE(is.list(split))) {
    is_stop("Input for `split` must be a 'list' with `to` and `from`")
  }

  if (isFALSE(is(dt, "data.table"))) data.table::setDT(dt)

  dt <- is_split_check(dt, split)

  if (!is.na(split$from)) {
    dt[, (split$to) := data.table::tstrsplit(get(split$from), split = "", fixed = TRUE)]
  }
  invisible(dt)
}

#' @title Get Split Column
#' @description Get the column to be splitted and the column name
#'   for the new splitted columns.
#'   If you already have the specification from [find_spec()], then the arguments
#'   for \code{group} and \code{con} must be \code{NULL}.
#' @inheritParams make_file
#' @inheritParams find_spec
#' @param spec Specification from \code{tbl_Filgruppe}
#' @return A list consist of two variables ie. \code{from} and \code{to}
#'   indicating which column to split and what the new column names will be
#' @export

get_split <- function(group = NULL, con = NULL, spec = NULL) {
  is_null_both(group, spec)
  is_not_null_both(group, spec)

  if (is.null(spec)) {
    spec <- find_spec("filegroups.sql", group, con)
  }

  from <- find_column_input(spec, "SPLITTFRA")
  to <- find_column_input(spec, "SPLITTTIL")

  valto <- is_separate(to, ",")

  return(list(from = from, to = valto))
}


## Helper -------------------
is_split_check <- function(dt, split){

  frm <- split$from
  fval <- unique(dt[[frm]])
  fnr <- nchar(fval)

  sto <- length(split$to)

  if (sto < max(fnr, na.rm = TRUE)){
    msgSplit <- paste0("Woops!!! SPLITTRA contains more than `", sto, "` variable when split. Check original file! \n")
    msgCode <- "Update SPLITTTIL or you may use this command to check the original file:"

    stop(is_colour_txt(x = sprintf("dt[, .N, by = %s]", frm), msg = paste0(msgSplit, msgCode), type = "error2"))
  }

  valdx <- which(fnr < sto)
  for (i in valdx){
    val <- fval[i]
    valDup <- paste(rep(val, sto), collapse = "")
    dt[get(frm) == val, (frm) := valDup]
  }

  invisible(dt)
}
