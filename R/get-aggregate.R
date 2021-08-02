#' Aggregate Data
#' @description
#' Get the specification on how the data will be aggregated to
#' different geographical levels ie. county, manucipality, town etc.
#' @inheritParams read_org
#' @inheritParams find_spec
#' @export
get_aggregere <- function(group = NULL, con = NULL) {
  is_null(group)
  is_null(con)
  spec <- find_spec("aggregate.sql", group, con)
  input <- find_column_input(spec, "AGGREGERE")
  is_separate(input, sep = ",")
}
