#' @title Create Georgraphical Codes
#' @description Create a database for geographical codes to aggregate
#'   data accordingly. Implementation of this function is base on [norgeo::cast_geo()]
#'   function in \href{https://helseprofil.github.io/norgeo/}{norgeo} package.
#' @param year Year for the valid geographical codes
#' @param write Write table to the `orgdata.geo` database. It will overwrite
#'    the table if it already exists
#' @param append Append the data to an existing table in the `orgdata.geo`
#' @param table Table name to be created in the database. Default is `tblGeo`
#' @importFrom norgeo cast_geo
#' @export
read_geo <- function(year = NULL, append = FALSE, write = FALSE, table = "tblGeo"){
  is_null(year)

  geoFile <- is_path_db(getOption("orgdata.geo"), check = TRUE)
  geo <- KHelse$new(geoFile)
  on.exit(geo$db_close(), add = TRUE)

  geo$tblvalue <- norgeo::cast_geo(year = year)
  geo$tblname <- table

  write <- is_write(write, table, geo$dbconn)
  if (write){
    geo$db_write(write = write)
    message("Write table `", table, "` is completed in: \n", geoFile)
  }

  if (append){
    geo$db_write(append = append)
    message("Append data to `", table, "` is completed in: \n", geoFile)
  }

  invisible(geo$tblvalue)
}

## Helper -----------------------------------------------------
# Warn user if table exists incase it's a mistake
is_write <- function(write, table, con){

  tblExist <- DBI::dbExistsTable(conn = con, name = table)
  if (isTRUE(write) && isTRUE(tblExist)){
    msgs <- sprintf("Table `%s` exists. Do you want to overwrite?", table)
    write <- utils::askYesNo(msg = msgs)
  }
  return(write)
}
