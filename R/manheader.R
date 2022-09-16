#' @title Rename Colname Manually
#' @description Rename the columns manually based on column index.
#' @inheritParams do_split
#' @param manspec Specification from \code{MANHEADER} column in \code{tbl_Innlesing}
#' @import data.table
#' @export
do_manheader <- function(dt = NULL, manspec = NULL) {
  is_debug()
  is_null(dt, "Data set not found!")
  is_null(manspec)

  varLength <- length(manspec)
  if (varLength == 2) {
    ## indx <- as.integer(manspec[["old"]])
    indx <- tryCatch({
      as.numeric(manspec[["old"]])
    },
    warning = function(wr){
      colx <- trimws(manspec[["old"]])
      vapply(colx, function(x) grep(x, names(dt)), numeric(1))
    },
    error = function(er){
      colx <- trimws(manspec[["old"]])
      varsDT <- sapply(colx, function(x) grep(x, names(dt), value = TRUE))
      for (i in seq_len(length( varsDT ))){
        msg <- paste0("Columnames in the dataset to rename `", names(varsDT)[i], "`:")
        is_color_txt(varsDT[i], msg)
      }
      print(manspec)
      is_stop("Check MANHEADER! Columnames to rename must be unique")
    })

    data.table::setnames(dt, names(dt)[indx], manspec[["new"]])
  }
  return(dt)
}


#' @title Get MANHEADER
#' @description Get the inputs for column MANHEADER in \code{tbl_Innlesing}.
#'    The input tells that we want to manually \strong{rename} the column for
#'    various reasons such as column name in the rawdata is too long or
#'    it uses unstandard naming style. An input like this:
#'
#' \code{3,6=AGE,EDUCATION}
#'
#' means we want to rename column \code{3} to \code{AGE} and column \code{6}
#' to \code{EDUCATION}.
#' @inheritParams make_file
#' @inheritParams find_spec
#' @inheritParams find_column_input
#' @return A list containing \code{$index} to refer to the column index and
#'     \code{$col} for the new name of the selected column index.
#' @export
get_manheader <- function(group = NULL, con = NULL, spec = NULL) {
  is_null_both(group, spec)
  is_not_null_both(group, spec)

  if (is.null(spec)) {
    spec <- find_spec("specification.sql", group, con)
  }

  input <- find_column_input(spec, "MANHEADER")

  if (!is.na(input)) {
    input <- is_col_separate(input = input)
  }
  return(input)
}

