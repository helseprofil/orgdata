#' Aggregate Data
#' @description
#' Get the specification on how the data will be aggregated to
#' different geographical levels ie. county, manucipality, town etc.
#' @inheritParams read_org
#' @inheritParams get_split
#' @inheritParams find_column_input
#' @export
get_aggregere <- function(group = NULL, con = NULL, spec = NULL) {
  is_null_also(group, spec)
  is_null_both(group, spec)

  if (is.null(spec)) {
    spec <- find_spec("filegroups.sql", group, con)
  }
  input <- find_column_input(spec, "AGGREGERE")
  is_separate(input, sep = ",")
}
