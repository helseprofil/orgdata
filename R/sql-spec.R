#' @title File Specifications in Registration Database
#'
#' @description
#' This function will find all specifications in the Access registration database via SQL code.
#' For example where the orginal data is and how the data will be restructured
#' and aggregated.. etc.. etc.. The specifications are registered in
#' the following tables:
#' \enumerate{
#'   \item{\strong{ tbl_Filgruppe }} - File group specification for the output
#'   \item{\strong{ tbl_Orgfile }} - The original files
#'   \item{\strong{ tbl_Innlesing }} - How the file will be read into R
#'   \item{\strong{ tbl_Koble }} - Connection for original files to the file groups and how these will be read
#'   \item{\strong{ tbl_KodeBok }} - Code book to recode any value
#'   \item{\strong{ tbl_Compute }} - Code book to create a new category from the existing categories
#' }
#'
#' @description SQL file must be written with \code{base::sprintf} style ie.
#'   \code{'%s','%d'} etc. Please refer to \code{base::sprintf} documentation.
#'   In addition, SQL code must not contain comments. Example of SQL code with
#'   \code{'%s'}: \code{SELECT * FROM tbl_Koble WHERE FILGRUPPE = '%s'} Which is
#'   saved in \code{C:/myfile.sql} and run as in the code example.
#' @param file SQL file. If external is TRUE then complete filepath must be
#'   specified.
#' @param value The value for selection in SQL code with \code{base::sprintf}
#'   style. For example the name of a \emph{filgruppe}.
#' @param con Connection to database
#' @param external If the SQL file is outside of the package. Default is
#'   \code{FALSE}.
#' @param char First input value to be added in the query
#' @param char2 Second input value to be added in the query
#' @param char3 Third input value to be added in the query
#' @param opposite TRUE if second input value will be read before first input value
#' @return Out put will be a data.frame.
#' @examples
#' \dontrun{
#' qr <- find_spec("C:/myfile.sql", value = "BEFOLKNING", con = dbconn, external = TRUE)
#' qr2 <- find_spec("your.sql", con = dbconn, char = "BEFOLKNING", char2 = 14)
#' }
#' @export
find_spec <- function(file = NULL,
                      value = NULL,
                      con = NULL,
                      external = FALSE,
                      char = NULL,
                      char2 = NULL,
                      char3 = NULL,
                      opposite = FALSE) {

  is_null(file)
  is_not_null_both(value, char)
  is_not_null_both(value, char2)
  is_null(con)

  qs <- is_query(file = file,
                 value = value,
                 external = external,
                 char = char,
                 char2 = char2,
                 char3 = char3,
                 opposite = opposite)
  DBI::dbGetQuery(con, qs)
}


#' @keywords internal
#' @title Create the SQL query
#' @description Create the SQL query to get the data as specified
is_query <- function(...) {

  args <- list(...)
  path <- system.file(args$file, package = "orgdata")

  exFile <- is.element("external", names(args))
  if (!exFile){
    args$external <- FALSE
  }

  if (args$external) {
    path <- args$file
  }

  txt <- paste(readLines(path), collapse = "\n")
  txt <- is_sql_code(txt)

  if (!is.null(args$value)) {
    qry <- sprintf(txt, args$value)
  } else {
    qry <- is_quick_fix(txt = txt,
                        char = args$char,
                        char2 = args$char2,
                        char3 = args$char3,
                        opposite = args$opposite)
  }

  return(qry)
}

## Helper ----------------------------------------------------
# Quick fix issue 309
is_quick_fix <- function(...){
  args <- list(...)
  if (!is.null(args$char3)){
    out <- sprintf(fmt = args$txt, char = args$char, char2 = args$char2, char3 = args$char3)
  } else {
    out <- is_opposite(txt = args$txt, char = args$char, char2 = args$char2, opposite = args$opposite)
  }

  return(out)
}

is_opposite <- function(txt, char, char2, opposite){
  if (opposite){
    out <- sprintf(txt, char2, char)
  } else {
    out <- sprintf(txt, char, char2)
  }

  return(out)
}



## SQL code need sprintf for dynamic query
is_sql_code <- function(x) {
  ## x : file with sql code
  if (grepl("%", x) != 1) {
    stop(simpleError("Missing `sprintf` reference in SQL code"))
  }

  invisible(x)
}
