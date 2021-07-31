#' R6 Class Connecting to Database
#'
#' @description
#' Connect to registration database to get all necessary information
#' on data source and cleaning specification. The driver is only applicable
#' to an Access Database.
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
    #' kh <- KHelse$new(file.path(osDrive, getOption("orgdata.folder"), getOption("orgdata.file")))
    #' kh$dbname
    #' kh$db_close()
    #' kh$db_connect()
    #' }
    initialize = function(dbname = NULL) {
      if (is.null(dbname)) {
        stop(message(" Woopss!! Database file is missing!"))
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
    db_write = function(name, value) {
      self$tblname <- name
      self$tblvalue <- value
      DBI::dbWriteTable(self$dbconn,
                        self$tblname,
                        self$tblvalue,
                        batch_rows = 1,
                        overwrite = TRUE
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
