
#' @title Codebook with Regular Expression
#' @description Get the codebook with regular expression to recode variables
#'   based on the `FILGRUPPE` and `LESID` number. Specification group `ALLE`
#'   will be used when neither `FILGRUPPE` nor `LESID` is specified.
#' @inheritParams find_column_input
#' @inheritParams find_spec
#' @family recode functions
#' @export
get_codebok_regexp <- function(spec = NULL, con = NULL){
  grp <- spec$FILGRUPPE
  lesid <- spec$LESID
  speCode <- find_spec("recode-regexp.sql", con = con, char = grp, char2 = lesid)
  is_codebook(cb = speCode)
}
