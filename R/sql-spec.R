#' Get File Specification
#'
#' @description
#' This function will access all specifications on where the orginal data is and how
#' the data will be restructured and aggregated. The specifications are registered in
#' the following register database:
#' \enumerate{
#'   \item{tbl_Filgruppe}
#'   \item{tbl_Koble}
#'   \item{tbl_Orgfile}
#'   \item{tbl_Innlesing}
#' }
#'
#' SQL file must be written with \code{base::sprintf} style ie. \code{'%s','%d'} etc.
#' Please refer to \code{base::sprintf} documentation. In addition, SQL code must not contain comments.
#' Example of SQL code with \code{'%s'}:
#' \code{SELECT * FROM tbl_Koble WHERE FILGRUPPE = '%s'}
#' Which is saved in \code{C:/myfile.sql} and run as in the code example.
#' @param file SQL file. If external is TRUE then complete filepath must be specified.
#' @param value The value for selection in SQL code with \code{base::sprintf} style.
#'    For example the name of a \emph{filgruppe}.
#' @param con Connection to database
#' @param ... Other arguments
#' @return Out put will be a data.frame.
#' @export
get_spec <- function(file = NULL, value = NULL, con = NULL, ...) {
  check_null(con)

  qs <- get_query(file, value, ...)
  dt <- DBI::dbGetQuery(con, qs)
  return(dt)
}


#' @export
#' @rdname get_spec
#' @param external If SQL file is outside of the package. Default is \code{FALSE}.
#' @examples
#' \dontrun{
#' qr <- get_query("C:/myfile.sql", value = "BEFOLKNING", external = TRUE)
#' }
get_query <- function(file = NULL, value = NULL, external = FALSE) {
  check_null(file)
  check_null(value)

  path <- system.file(file, package = "orgdata")

  if (external) {
    path <- file
  }

  txt <- paste(readLines(path), collapse = "\n")
  check_sql(txt)
  query <- sprintf(txt, value)
}
