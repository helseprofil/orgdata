#' File Specifications in Registration Database
#'
#' @description
#' This function will find all specifications in the Access registration database via SQL code.
#' For example where the orginal data is and how the data will be restructured
#' and aggregated.. etc.. etc.. The specifications are registered in
#' the following tables:
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
#' @param external If the SQL file is outside of the package. Default is \code{FALSE}.
#' @param num Which value are numeric
#' @param char Which value are character
#' @param opposite TRUE if numeric value will be read before character
#' @return Out put will be a data.frame.
#' @export
find_spec <- function(file = NULL,
                      value = NULL,
                      con = NULL,
                      external = FALSE,
                      char = NULL,
                      num = NULL,
                      opposite = FALSE) {
  is_null(con)
  qs <- find_query(file, value, external, char, num, opposite)
  DBI::dbGetQuery(con, qs)
}


#' @export
#' @rdname find_spec
#' @examples
#' \dontrun{
#' qr <- find_query("C:/myfile.sql", value = "BEFOLKNING", external = TRUE)
#' qr2 <- find_query("your.sql", char = "BEFOLKNING", num = 14)
#' }
find_query <- function(file = NULL,
                       value = NULL,
                       external = FALSE,
                       char = NULL,
                       num = NULL,
                       opposite = FALSE) {
  is_null(file)

  path <- system.file(file, package = "orgdata")

  if (external) {
    path <- file
  }

  txt <- paste(readLines(path), collapse = "\n")
  is_sql_code(txt)

  if (!is.null(value)) {
    sprintf(txt, value)
  } else {
    is_opposite(txt, char, num, opposite)
  }
}

## Helper ----------------------------------------------------

is_opposite <- function(txt, char, num, opposite){
  if (opposite){
    sprintf(txt, num, char)
  } else {
    sprintf(txt, char, num)
  }
}

## SQL code need sprintf for dynamic query
is_sql_code <- function(x) {
  ## x : file with sql code
  if (grepl("%", x) != 1) {
    stop("Missing `sprintf` reference in SQL code")
  }
}
