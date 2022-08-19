#' @title See Structured Data in the Data Warehouse
#' @description See the original data that have been clean and structured i the
#'   data warehouse. Data is saved in the warehouse when the column
#'   *KONTROLLERT* is marked in the original file table in Access. This means
#'   the dataset has been cleaned and recoded as specified in *INNLESING* table
#'   in Access registration database.
#' @param group The filegroup name (\emph{filgruppe})
#' @inheritParams make_file
#' @param action To read or delete the data in the warehouse. Default is `read`.
#' @examples
#' \dontrun{
#' dt <- see_data("LESEFERD", koblid = 134)
#' }
#' @export
see_data <- function(group = NULL, koblid  = NULL, year = NULL, action = c("read", "delete")){

  action <- match.arg(action)
  if (is.null(year))
    year <- getOption("orgdata.year")

  if (length(action) > 1) action = "read"
  is_null_both(group, koblid, msg = "Both args can't be empty!")

  duckPath <- is_path_db(getOption("orgdata.folder.org.db"))
  duckFile <- file.path(duckPath,
                        getOption("orgdata.year"),
                        paste0( group, ".duckdb" ))
  if(!(fs::is_file(duckFile))){
    is_stop("Database file not found for FILGRUPPE:", group)
  }

  rcon <- is_conn_db(dbname = group, db = "raw", dbyear = year)
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
