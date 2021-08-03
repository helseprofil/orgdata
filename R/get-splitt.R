#' @title Split Column
#' @description Split the columns as specified in the registration database.
#' @param dt Input data
#' @param split Split specification as a list. Should be equivalent
#'    to the output of [get_split()] function
#' @export
do_split <- function(dt = NULL, split = NULL) {
  is_null(dt, "Data is not found!")
  is_null(split)

  if (isFALSE(is.list(split))) {
    stop("Input for `split` must be a 'list' with `to` and `from`")
  }

  if (isFALSE(class(dt)[1] == "data.table")) data.table::setDT(dt)

  if (!is.na(split$from)) {
    dt[, (split$to) := data.table::tstrsplit(get(split$from), split = "", fixed = TRUE)]
  }
  return(dt)
}

#' @title Get Split Column
#' @description Get the column to be splitted and the column name
#'   for the new splitted columns.
#'   If you already have the specification from [find_spec()], then the arguments
#'   for \code{group} and \code{con} must be \code{NULL}.
#' @inheritParams read_org
#' @inheritParams find_spec
#' @param spec Specification data from register database
#' @return A list consist of to variable ie. \code{from} and \code{to}
#'   indicating which column to split and what the new column name will be
#' @export

get_split <- function(group = NULL, con = NULL, spec = NULL) {
  is_null_also(group, spec)
  is_not_both(group, spec)

  if (is.null(spec)) {
    spec <- find_spec("filegroups.sql", group, con)
  }

  from <- find_column_input(spec, "SPLITTFRA")
  to <- find_column_input(spec, "SPLITTTIL")

  valto <- is_separate(to, ",")

  return(list(from = from, to = valto))
}
