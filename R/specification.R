#' Get File Specification
#'
#' @description
#' This function will access all specifications on where the orginal data is and how
#' the data will be restructured and aggregated. The specifications are registered in
#' the following register database:
#' \enumerate{
#'   \item{tbl_Koble}
#'   \item{tbl_Orgfile}
#'   \item{tbl_Innlesing}
#' }
#' @param file SQL file. If external is TRUE then full file must be specified.
#' @param filgruppe The \emph{filgruppe} of files category
#' @param con Connection to database
#' @return A data.frame
#' @export
get_spec <- function(file = NULL, filgruppe = NULL, con = NULL) {
  qs <- get_query("specification.sql", filgruppe)
  dt <- DBI::dbGetQuery(con, qs)
  data.table::setDT(dt)
}


#' @export
#' @rdname get_spec
#' @inheritParams get_spec
#' @param value The value for selection in SQL code with \code{base::sprintf} style.
#' @param external If SQL file is outside of the package. Default it \code{FALSE}.
get_query <- function(file = NULL, value = NULL, external = FALSE) {
  if (is.null(value)) {
    stop("Value is missing")
  }

  path <- system.file(file, package = "orgdata")

  if (external) {
    path <- file
  }

  txt <- paste(readLines(path), collapse = "\n")
  query <- sprintf(txt, value)
}
