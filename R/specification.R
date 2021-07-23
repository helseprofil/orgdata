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
#' @param filgruppe The \emph{filgruppe} of files category
#' @param con Connection to database
#' @return A data.frame
#' @export
get_spec <- function(filgruppe = NULL, con = NULL) {
  qs <- sprintf("SELECT KOBLID, tbl_Koble.FILID, tbl_Koble.FILGRUPPE, FILNAVN, IBRUKTIL, tbl_Innlesing.*
                 FROM tbl_Innlesing
                 INNER JOIN (tbl_Koble
                       INNER JOIN tbl_Orgfile
                       ON tbl_Koble.FILID = tbl_Orgfile.FILID)
                 ON (tbl_Innlesing.LESID = tbl_Koble.LESID)
                 WHERE tbl_Koble.FILGRUPPE = '%s'
                 AND tbl_Orgfile.IBRUKTIL = #9999-01-01#", filgruppe)

  dt <- DBI::dbGetQuery(con, qs)
  data.table::setDT(dt)
}
