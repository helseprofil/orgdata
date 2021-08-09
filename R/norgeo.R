#' @title Create Georgraphical Codes
#' @description Create a database for geographical codes to aggregate
#'   data accordingly. Implementation of this function is base on [norgeo::cast_geo()]
#'   function in \href{https://helseprofil.github.io/norgeo/}{norgeo} package.
#' @param year Year for geographical codes
#' @param write Write table `tblGeo` to the `orgdata.geo` database. It will overwrite
#'    the `tblGeo` if it already exists
#' @param append Append the data to an existing `tblGeo` in the `orgdata.geo`
#' @export
do_norgeo <- function(year = NULL, write = FALSE, append = FALSE){
  is_null(year)

  geoFile <- is_path_db(getOption("orgdata.geo"), check = TRUE)
  geo <- KHelse$new(geoFile)
  on.exit(geo$db_close(), add = TRUE)

  geo$tblvalue <- norgeo::cast_geo(year = year)
  geo$tblname <- "tblGeo"

  if (write){
    geo$db_write(write = write)
    message("Write table `tblGeo` is completed. \n", geoFile)
  }

  if (append){
    geo$db_write(append = append)
    message("Append data to `tblGeo` is completed. \n", geoFile)
  }

  invisible(geo$tblvalue)
}
