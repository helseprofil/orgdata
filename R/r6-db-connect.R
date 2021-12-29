#' @title Connect to Database
#' @description Use R6 object to connect to database
#' @param db Database file `kh` (Kommunehelse) and `geo` (Geo code)
#' @param .test Use for testing only
#' @keywords internal
is_conn_db <- function(dbname = NULL, db = c("kh", "geo"), .test = FALSE){

  db <- match.arg(db)
  dbfile <- switch(db,
                   kh = getOption("orgdata.db"),
                   geo = getOption("orgdata.geo"),
                   getOption("orgdata.db"))

  if (is.null(dbname)){
    dbname <- is_path_db(
      db = dbfile,
      check = TRUE
    )
  }

  if (.test){
    return(dbname)
  }

  KHelse$new(dbname = dbname)
}

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
    #' @examples
    #' \dontrun{
    #' kh <- KHelse$new(file.path(getOption("orgdata.drive"),
    #'                            getOption("orgdata.folder.db"),
    #'                            getOption("orgdata.db")))
    #' kh$dbname
    #' kh$db_close()
    #' kh$db_connect()
    #' }
    initialize = function(dbname = NULL) {
      if (is.null(dbname)) {
        stop(message(" Woopss!! Can't find database file!"))
      } else {
        self$dbname <- dbname
        cs <- paste0(private$..drv, self$dbname)
        self$dbconn <- DBI::dbConnect(odbc::odbc(),
                                      .connection_string = cs,
                                      encoding = "latin1"
                                      )
      }
    },

    #' @description
    #' Reconnect to the database when \code{db_close} was used.
    db_connect = function() {
      stopifnot(!is.null(self$dbname))
      cs <- paste0(private$..drv, self$dbname)
      self$dbconn <- DBI::dbConnect(odbc::odbc(),
                                    .connection_string = cs,
                                    encoding = "latin1"
                                    )
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

      DBI::dbWriteTable(self$dbconn,
                        self$tblname,
                        self$tblvalue,
                        batch_rows = 1,
                        overwrite = write,
                        append = append,
                        field.types = field.types
                        )
    },

    #' @description
    #' Close connection to the database.
    db_close = function() {
      DBI::dbDisconnect(self$dbconn)
    }
  ),
  private = list(
    ..drv = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
    finalize = function() {
      DBI::dbDisconnect(self$dbconn)
    }
  )
)
