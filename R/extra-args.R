#' @title Execute Extra Arguments
#' @description This is based on the input in `EXTRA` column from Access
#'   registration database. The arguments that are valid here can be expanded
#'   whenever needed. See details section for valid arguments to be used.
#'   All argument names are written in `CamelCase` style.
#'
#' @details Currently, these arguments can be used:
#'   - `DeleteNaRow` : Delete any row consisting only NA in all columns
#' @param dt Dataset
#' @param args Extra arguments as specified in details section below.
#' @family extra arguments
#' @export
do_extra_args <- function(dt = NULL, args = NULL){
  dt <- is_delete_na_row(dt, extra = args)
}

#' @title Get Extra Arguments
#' @description This is based on the input in `EXTRA` column from Access registration database.
#'   The arguments that are valid here can be expanded whenever needed. All argument names are written
#'   in `CamelCase` style. Use comma to seperate multiple arguments.
#' @inheritParams make_file
#' @inheritParams find_spec
#' @inheritParams find_column_input
#' @return A list with the names and value of arguments
#' @family extra arguments
#' @export

get_extra_args <- function(group = NULL, con = NULL, spec = NULL){

  is_debug()
  is_null_both(group, spec)
  is_not_null_both(group, spec)

  if (is.null(spec)) {
    spec <- find_spec("specification.sql", value = group, con = con)
  }

  input <- find_column_input(spec = spec, col = "EXTRA")

  if (!is.na(input)) {
    input <- find_column_multi(spec = spec, col = "EXTRA", sep = ",")
  }
  return(input)

}

## Helper ---------------------------
## Delete all row with NA
## extra - Extra arguments from get_extra_args()
is_delete_na_row <- function(dt = NULL, extra = NULL){
  nrc <- NULL

  delRow <- sum(is.element(extra, "DeleteNaRow"))
  if (delRow){
    dt <- is_null_to_na(dt)
    nrCol <- ncol(dt)
    dt[, nrc := rowSums(is.na(.SD))]
    dt <- dt[nrc!=nrCol, ]
    dt[, nrc := NULL]
  }

  return(dt)
}

## Whitespace to NA
is_null_to_na <- function(dt){
  for (j in seq_len(ncol(dt))){
    if(is(dt[[j]], "character")){
      data.table::set(dt, j = j, value = trimws(dt[[j]]))
    }
    data.table::set(dt, i=which(dt[[j]]==""), j = j, value = NA)
  }

  return(dt)
}
