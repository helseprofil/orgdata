#' @title See Original Data in the Database
#' @description See the original data that are saved in the org database when
#'   the column *KONTROLLERT* is marked. This means the dataset has been cleaned
#'   and recoded as specified in *INNLESING* table in Access registration
#'   database.
#' @param name The filegroup name
#' @inheritParams make_file
#' @param action To read or delete the data in the database. Default is `read`.
#' @examples
#' \dontrun{
#' dt <- see_org("LESEFERD", koblid = 134)
#' }
#' @export
see_org <- function(name = NULL, koblid  = NULL, action = c("read", "delete")){

  action <- match.arg(action)
  if (length(action) > 1) action = "read"
  is_null_both(name, koblid, msg = "Both args can't be empty!")

  duckPath <- is_path_db(getOption("orgdata.folder.org.db"))
  duckFile <- file.path(duckPath,
                        getOption("orgdata.year"),
                        paste0( name, ".duckdb" ))
  if(!(fs::is_file(duckFile))){
    is_stop("Database file not found for FILGRUPPE:", name)
  }

  rcon <- is_conn_db(dbname = name, db = "raw")
  on.exit(rcon$db_close(), add = TRUE)

  dbTables <- DBI::dbListTables(rcon$dbconn)
  dbTables <- vapply(dbTables, as.integer, integer(1))

  if (isFALSE(any(koblid %in% dbTables))){
    message("Available koblid:", is_short_code(dbTables))
    is_stop("Not found KOBLID:", koblid)
  }

  if (action == "delete"){
    rcon$db_remove_table(as.character(koblid))
    is_color_txt(koblid, "Delete raw database for KOBLID:")
    invisible()
  } else {
    dt <- rcon$db_read(as.character(koblid))
    data.table::setDT(dt)
    dt[]
  }
}
