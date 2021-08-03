#' @title Rename Standard Columns
#' @description Renaming standard columns as in `getOption("orgdata.columns")`.
#' @param file Input data
#' @inheritParams get_year
#' @export
do_rename_col_standard <- function(file = NULL, spec = NULL) {
  is_null(file)
  is_null(spec, "Specification to rename columns is missing")

  dt <- read_file(file)
  cols <- find_column_standard(spec)
  data.table::setnames(dt, cols$old, cols$new)
}
