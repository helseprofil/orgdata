#' @title Connecting to Database
#' @description
#' Connect to registration database to get all necessary information
#' on data source and cleaning specification. The driver is only applicable
#' to an Access Database. Calling the method \code{KHelse$new(YourFilePath)}
#' will create an object of R6 Class. Please refer to the
#' \href{https://helseprofil.github.io/orgdata/reference/KHelse.html#examples}{examples}.
#' @export
KHelse <- R6::R6Class(
  classname = "KHelse",
  cloneable = FALSE,
  public = list(

    #' @field dbname Database filename.
    dbname = NULL,

    #' @field dbtype Database type of either Access or DuckDB
    dbtype = NULL,

    #' @field dbyear Production year. Only relevant for DuckDB
    dbyear = NULL,

    #' @field dbconn Database connection.
    dbconn = NULL,

    #' @field tblname Table name to be created in the database.
    tblname = NULL,

    #' @field tblvalue Data to be inserted in the table \code{tblname}.
    #'   Data must be in a \code{data.frame} or \code{data.table} format.
    tblvalue = NULL,

    #' @description
    #' Start connecting to the database.
    #' @param dbname Database filename.
    #' @param dbtype Database type eg. Access, SQLite, DuckDB etc.
    #' @param dbyear Production year. This arg only relevant to raw database in DuckDB
    #' @examples
    #' \dontrun{
    #' kh <- KHelse$new(file.path(getOption("orgdata.drive"),
    #'                            getOption("orgdata.folder.db"),
    #'                            getOption("orgdata.db")))
    #' kh$dbname
    #' kh$db_close()
    #' kh$db_connect()
    #' }
    initialize = function(dbname = NULL, dbtype = "Access", dbyear = NULL) {

      if (is.null(dbname)) {
        stop(message(" Woopss!! Can't find database file!"))
      } else {

        self$dbname <- dbname
        self$dbtype <- dbtype
        self$dbyear <- dbyear
        self$dbconn <- connect_db(dbname = self$dbname,
                                  dbtype = self$dbtype,
                                  dbyear = self$dbyear,
                                  dbdriver = private$..drv)
      }
    },

    #' @description
    #' Reconnect to the database when \code{db_close} was used.
    db_connect = function() {
      stopifnot(!is.null(self$dbname))
      self$dbconn <- connect_db(dbname = self$dbname,
                                dbtype = self$dbtype,
                                dbyear = self$dbyear,
                                dbdriver = private$..drv)
    },

    #' @description
    #' Write table to the database.
    #' @param name Table name to be created in the database.
    #' @param value The data to be inserted in the table.
    #' @param write Write a table to the database. It will overwrite
    #'    the table if it already exists
    #' @param append Append the data to an existing table in the database
    #' @param field.types Type of data in specified column. Must be named as vector
    db_write = function(name = NULL, value=NULL, write = FALSE, append = FALSE, field.types = NULL) {
      if(!is.null(name)) { self$tblname <- name }
      if(!is.null(value)) { self$tblvalue <- value }

      write_db(name = self$tblname,
               dbconn = self$dbconn,
               value= self$tblvalue,
               write = write,
               append = append,
               field.types = field.types,
               dbtype = self$dbtype)
    },

    #' @description
    #' Read table and convert to data.table format
    #' @param name Table name to be created in the database.
    db_read = function(name = NULL){
      if(!is.null(name)) {self$tblname <- name}
      DT <- DBI::dbReadTable(self$dbconn, name = self$tblname)
      data.table::setDT(DT)
    },

    #' @description
    #' Remove table in the database.
    #' @param name Table name to be created in the database.
    db_remove_table = function(name = NULL){
      if(!is.null(name)) { self$tblname <- name }
      DBI::dbRemoveTable(self$dbconn, self$tblname)
    },

    #' @description
    #' Close connection to the database.
    db_close = function() {
      if (self$dbtype == "Access"){
        DBI::dbDisconnect(self$dbconn)
      }

      if (self$dbtype == "DuckDB"){
        DBI::dbDisconnect(self$dbconn, shutdown = TRUE)
      }
    }
  ),
    private = list(
      ..drv = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
      finalize = function() {
        if (self$dbtype == "Access"){
          DBI::dbDisconnect(self$dbconn)
        }

        if (self$dbtype == "DuckDB"){
          DBI::dbDisconnect(self$dbconn, shutdown = TRUE)
        }
      }
    )
)

#' @title Connect to Database
#' @description Use R6 object to connect to database
#' @param dbname Database filename with complete path
#' @param db Database file `kh` (Kommunehelse), `geo` (Geo code) or `raw` (Raw database)
#' @param .test Use for testing only
#' @param ... Other arguments
#' @keywords internal
is_conn_db <- function(dbname = NULL, db = c("kh", "geo", "raw"), .test = FALSE, ...){

  db <- match.arg(db)
  dbfile <- switch(db,
                   kh = getOption("orgdata.db"),
                   geo = getOption("orgdata.geo"),
                   getOption("orgdata.db"))

  if (is.null(dbname)){
    dbname <- is_path_db(db = dbfile, ...)
  }

  if (.test){
    return(dbname)
  }

  if (db == "raw"){
    KHelse$new(dbname = dbname, dbtype = "DuckDB", dbyear = getOption("orgdata.year"), ...)
  } else {
    KHelse$new(dbname = dbname, ...)
  }
}


## HELPER ---------------
connect_db <- function(dbname, dbtype, dbyear, dbdriver){
  switch(dbtype,
         Access = {
           DBI::dbConnect(odbc::odbc(),
                          .connection_string = paste0(dbdriver, dbname),
                          encoding = "latin1")
         },
         DuckDB = {
           duckFile <- paste0(dbname, ".db")
           duckPath <- is_path_db(getOption("orgdata.folder.org.db"))
           duckRoot <- file.path(duckPath, dbyear)
           if (!fs::dir_exists(duckRoot)){
             fs::dir_create(duckRoot)
           }

           DBI::dbConnect(RSQLite::SQLite(), file.path(duckRoot, duckFile))
         })

}

write_db <- function(name = NULL,
                     dbconn = NULL,
                     value = NULL,
                     write = FALSE,
                     append = FALSE,
                     field.types = NULL,
                     dbtype = NULL){

  switch(dbtype,
         Access = {
           DBI::dbWriteTable(conn = dbconn,
                             name = name,
                             value = value,
                             # https://github.com/r-dbi/odbc/issues/263
                             batch_rows = 1,
                             overwrite = write,
                             append = append,
                             field.types = field.types
                             )
         },
         DuckDB = {
           DBI::dbWriteTable(conn = dbconn,
                             name = name,
                             value = value,
                             overwrite = write)
         })
}
