#' @title See Structured Data in the Data Warehouse
#' @description See the original data that have been clean and structured i the
#'   data warehouse. Data is saved in the warehouse when the column
#'   *KONTROLLERT* is marked in the original file table in Access. This means
#'   the dataset has been cleaned and recoded as specified in *INNLESING* table
#'   in Access registration database.
#' @param group The filegroup name (\emph{filgruppe})
#' @param koblid Specify one or multiple KOBLID. Use `"all"` to delete all data in warehouse for seleted filegroup.
#' @inheritParams make_file
#' @param action To read or delete the data in the warehouse. Default is `read`.
#' @examples
#' \dontrun{
#' dt <- see_data("LESEFERD", koblid = 134)
#' dt <- see_data("ENPERSON", koblid = "all")
#' dt <- see_data("ENPERSON", koblid = 267:270, action = "delete")
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

  if (!any(koblid == "all"))
    is_check_tables(koblid, dbTables, group)

  if (action == "delete"){
    is_delete_tables(group, koblid, dbTables, conn = rcon)
  } else {
    dt <- is_read_tables(group, koblid, dbTables, conn = rcon)
    return(dt)
  }

  invisible()
}

## Helper -------------
is_read_tables <- function(group, koblid, dbTables, conn){

  if (any(koblid == "all")){
    is_color_txt(group, "Read all data in warehose for", type = "note")
    koblid <- dbTables
  } else {
    idTxt <- is_short_code(koblid)
    is_color_txt(idTxt, "Read data in warehouse for KOBLID:")
    koblid <- is_tables_name(koblid)
  }

  dt <- lapply(koblid, conn$db_read)
  dt <- data.table::rbindlist(dt)
  data.table::setDT(dt)
}

is_delete_tables <- function(group, koblid, dbTables, conn){

  if (any(koblid == "all")){
    is_color_txt(group, "Delete all data in warehose for", type = "warn")
    koblid <- dbTables
  } else {
    idTxt <- is_short_code(koblid)
    is_color_txt(idTxt, "Delete data in warehouse for KOBLID:")
    koblid <- is_tables_name(koblid)
  }

  lapply(koblid, conn$db_remove_table)
  invisible()
}

is_check_tables <- function(koblid, dbTables, group){

  if (length(dbTables) != 0){
    tblID <- is_tables_id(dbTables)
    msg <- "Available koblid:"
    txt <- is_short_code(tblID)
  } else {
    msg <- "No data found in the warehouse for"
    txt <- group
  }

  koblid <- is_tables_name(koblid)
  if (isFALSE(any(koblid %in% dbTables))){
    is_color_txt(txt, msg = msg)
    tblNum <- is_tables_id(koblid)
    is_stop("Not found for requested KOBLID:", tblNum)
  }

  invisible()
}

# Convert table names to numeric
is_tables_id <- function(tbls){
  vapply(tbls, function(x) as.numeric(sub("tbl_", "", x)), numeric(1))
}

is_tables_name <- function(tbls){
  paste0("tbl_", tbls)
}
