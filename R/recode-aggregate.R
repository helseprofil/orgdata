#' @title Recode Category Representing All
#' @description Recode the category for all after aggregating data based on the
#'  specification in Codebook. For example when aggregating category type of diagnoses
#'  `NA` will be produced to represent all type of diagnoses. This will basically
#'  recode `NA` to a preferred value such as `ALL`.
#' @inheritParams do_split
#' @inheritParams find_spec
#' @inheritParams make_file
#' @family recode functions
#' @export
do_recode_aggregate <- function(dt = NULL,
                                con = NULL,
                                aggregate = getOption("orgdata.aggregate")){
  is_debug()

  if (aggregate){
    speCode <- find_spec("recode-aggregate.sql", value = "AGGREGATE", con = con)
    dt <- is_recode_aggregate(dt = dt, code = speCode)
  }
  invisible(dt)
}

## Total value when aggregated
is_recode_aggregate <- function(dt, code){
  FILGRUPPE <- KOL <- FRA <- TIL <- NULL
  data.table::setDT(code)
  allCode <- code[FILGRUPPE == "AGGREGATE", list(KOL, FRA, TIL)]
  kols <- unique(allCode$KOL)

  notCols <- setdiff(kols, names(dt))
  if (length(notCols) > 0){
    message("Columname(s) defined in AGGREGERE for recoding not found: ", paste_cols(notCols))
  }

  yesCols <- intersect(kols, names(dt))
  if (length(yesCols) > 0){
    message("Columname(s) defined in AGGREGERE for recoding: ", paste_cols(yesCols))
    dt <- is_recode(dt = dt, code = allCode, cols = yesCols)
  }

  invisible(dt)
}
