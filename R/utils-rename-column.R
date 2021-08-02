#' @title Rename standard columns
#' @description Rename columns in the rawdata to the standard names
#'    as in `getOptions("orgdata.columns")`.
#' @inheritParams find_column_input
#' @return A list with `old` and `new` columnnames
#' @keywords internal
#' @import data.table
is_column_standard <- function(df) {
  GEO <- KJONN <- AAR <- ALDER <- UTDANN <- LANDBAK <- VAL <- NULL
  ## There are 7 standard columns
  for (i in seq_len(7)) {
    input <- is_column_name(df, getOption("orgdata.columns")[i])
    assign(input[["col"]], input)
  }
  x <- data.table::rbindlist(list(GEO, AAR, KJONN, ALDER, UTDANN, LANDBAK, VAL))

  old <- x[!is.na(x$input), ]$input
  new <- x[!is.na(x$input), ]$col
  list(old = old, new = new)
}


## Change dummy input to NA for easy selection
is_column_name <- function(df, col) {
  input <- find_column_input(df, col)
  dummy <- is_dummy(input)
  if (dummy) input <- NA
  list(col = col, input = input)
}
