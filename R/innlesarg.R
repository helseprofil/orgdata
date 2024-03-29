#' Get INNLESARG Specifications
#'
#' @description
#' This function will access all possible arguments in columns \code{INNLESARG}
#' in table \emph{tbl_Innlesing}'. Symbol \code{"|"} \strong{MUST} be used to
#' separate arguments if there are more than one arguments. This symbol is right on top
#' of `TAB` button on the keyboard.
#'
#' Arguments that can be used here depends on the file type. Please refer to [read_file()].
#'
#' For example:
#'
#' `header=TRUE | sheet=Ark1 | sep=,`
#' @inheritParams make_file
#' @inheritParams find_spec
#' @inheritParams find_column_input
#' @return A list with the names and value of arguments
#' @export
get_innlesarg <- function(group = NULL, con = NULL, spec = NULL) {
  # column INNLESARG must use '|' symbol as seperation btw arguments!
  is_debug()
  is_null_both(group, spec)
  is_not_null_both(group, spec)

  if (is.null(spec)) {
    spec <- find_spec("specification.sql", value = group, con = con)
  }

  input <- find_column_input(spec = spec, col = "INNLESARG")

  if (!is.na(input)) {
    args <- find_column_multi(spec = spec, col = "INNLESARG", sep = "|")
    input <- find_column_multi_input(input = args)
  }
  return(input)
}

