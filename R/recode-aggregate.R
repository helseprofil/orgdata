#' @title Recode Category Representing All Groups
#' @description Recode the category for all after aggregating data based on the
#'  specification in Codebook. For example when aggregating category type of diagnoses
#'  `NA` will be produced to represent all type of diagnoses. This will basically
#'  recode `NA` to a preferred value such as `ALL`.
#' @inheritParams do_split
#' @inheritParams find_column_input
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
    grp <- spec$FILGRUPPE
    lesid <- spec$LESID
    speCode <- get_codebok_aggregate(spec = spec, con = con)
    dt <- is_recode_lesid(dt = dt, code = speCode, lesid = lesid)
    dt <- is_recode_common(dt = dt, code = speCode, group = grp)
    dt <- is_recode_all(dt = dt, code = speCode, aggregate.msg = TRUE, control = control)
  }
  return(dt)
}


#' @title Codebook for Aggregate Columns
#' @description Get the codebook for recoding of aggregated variables based on the
#'  `FILGRUPPE` and `LESID` number. Specification group `ALLE` will be
#'  used when neither `FILGRUPPE` nor `LESID` is specified.
#' @inheritParams find_column_input
#' @inheritParams find_spec
#' @family recode functions
#' @export
get_codebok_aggregate <- function(spec = NULL, con = NULL){
    grp <- spec$FILGRUPPE
    lesid <- spec$LESID
    speCode <- find_spec("recode-aggregate.sql", con = con, char = grp, char2 = lesid)
    is_codebook(cb = speCode)
}

