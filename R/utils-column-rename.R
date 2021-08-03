#' @title Rename Standard Columns
#' @description Renaming standard columns as in `getOption("orgdata.columns")`.
#' @param file Input data
#' @inheritParams get_year
#' @export
do_column_standard_rename <- function(file = NULL, spec = NULL) {
  is_null(file)
  is_null(spec, "Specification to rename columns is missing")

  dt <- read_file(file)
  cols <- find_column_standard(spec)
  data.table::setnames(dt, cols$old, cols$new)
}

#' @title Find Standard Columns
#' @description Rename columns in the rawdata to the standard names
#'    as in `getOptions("orgdata.columns")`.
#' @inheritParams find_column_input
#' @return A list with `old` and `new` columnnames
#' @import data.table
#' @export
find_column_standard <- function(spec = NULL) {
  GEO <- KJONN <- AAR <- ALDER <- UTDANN <- LANDBAK <- VAL <- NULL

  is_null(spec)

  ## There are 7 standard columns
  for (i in seq_len(7)) {
    input <- is_column_name(spec, getOption("orgdata.columns")[i])
    assign(input[["col"]], input)
  }
  x <- data.table::rbindlist(list(GEO, AAR, KJONN, ALDER, UTDANN, LANDBAK, VAL))

  old <- x[!is.na(x$input), ]$input
  new <- x[!is.na(x$input), ]$col
  list(old = old, new = new)
}


## Change dummy input to NA for easy selection
is_column_name <- function(spec, col) {
  input <- find_column_input(spec, col)
  dummy <- is_dummy(input)
  if (dummy) input <- NA
  list(col = col, input = input)
}

## TODO How to implement MANHEADER
