#' @title Recode Category Representing All Groups
#' @description Recode the category for all after aggregating data based on the
#'  specification in Codebook. For example when aggregating category type of diagnoses
#'  `NA` will be produced to represent all type of diagnoses. This will basically
#'  recode `NA` to a preferred value such as `0` or `Total`.
#' @inheritParams do_split
#' @inheritParams get_split
#' @inheritParams find_spec
#' @inheritParams make_file
#' @inheritParams do_geo_recode
#' @family recode functions
#' @export
do_recode_aggregate <- function(dt = NULL,
                                spec = NULL,
                                con = NULL,
                                aggregate = getOption("orgdata.aggregate"),
                                control = FALSE) {
  is_debug()

  if (aggregate){
    grp <- find_column_input(spec = spec, "FILGRUPPE")
    speCode <- get_codebok_aggregate(spec = spec, con = con)
    dt <- is_recode_common(dt = dt, code = speCode, group = grp)
    dt <- is_recode_all(dt = dt, code = speCode, aggregate.msg = TRUE, control = control)
  }
  return(dt)
}


#' @title Codebook for Aggregate Columns
#' @description Get the codebook for recoding of aggregated variables based on
#'   the `FILGRUPPE`. Specification group `ALLE` will be used when `FILGRUPPE`
#'   is not specified.
#' @inheritParams find_column_input
#' @inheritParams find_spec
#' @family recode functions
#' @export
get_codebok_aggregate <- function(spec = NULL, con = NULL){
    grp <- find_column_input(spec = spec, "FILGRUPPE")
    speCode <- find_spec("recode-aggregate.sql", con = con, value = grp)
    is_codebook(cb = speCode)
}

