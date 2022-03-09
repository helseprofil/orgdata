#' @title See Raw Data in the Database
#' @description See the raw data that are saved in the raw database when the
#'   column *KONTROLLERT* is marked.
#' @param name The filegroup name
#' @param filid File ID for the raw data
#' @param action To read or delete the data in the database. Default is `read`.
#' @examples
#' \dontrun{
#' dt <- see_raw("LESEFERD", filid = 134)
#' }
#' @export
see_raw <- function(name = NULL, filid = NULL, action = c("read", "delete")){

  action <- match.arg(action)
  if (length(action) == 2) action = "read"

  is_null_both(name, filid, msg = "Both args can't be empty!")
  rcon <- is_conn_db(dbname = name, db = "raw")
  on.exit(rcon$db_close(), add = TRUE)

  dbTables <- DBI::dbListTables(rcon$dbconn)
  dbTables <- vapply(dbTables, as.integer, integer(1))

  if (isFALSE(any(filid %in% dbTables))){
    is_stop("Not found FILID:", filid)
  }

  if (action == "delete"){
    rcon$db_remove_table(as.character(filid))
    is_color_txt(filid, "Delete raw database for FILID:")
    invisible()
  } else {
    dt <- rcon$db_read(as.character(filid))
    data.table::setDT(dt)
    dt[]
  }
}
