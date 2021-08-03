#' @title Rename Standard Columns
#' @description Renaming standard columns as in `getOption("orgdata.columns")`.
#' @param file Input data
#' @inheritParams get_year
#' @export
do_rename_col_standard <- function(file = NULL, df = NULL) {
  is_null(file)
  is_null(df, "Specification to rename columns is missing")

  dt <- read_file(file)
  cols <- is_column_standard(df)
  data.table::setnames(dt, cols$old, cols$new)
}
