#' @title Granularity of Georgraphical Codes
#' @description Create a database granularity of geographical codes to aggregate
#'   data accordingly. Implementation of this function is base on [norgeo::cast_geo()]
#'   function in \href{https://helseprofil.github.io/norgeo/}{norgeo} package.
#' @param year Year for the valid geographical codes
#' @param write Write table to the `orgdata.geo` database. It will overwrite
#'    the table if it already exists
#' @param append Append the data to an existing table in the `orgdata.geo`
#' @param table Table name to be created in the database. Default is `tblGeo`
#' @importFrom norgeo cast_geo
#' @export
geo_level <- function(year = NULL, append = FALSE, write = FALSE, table = "tblGeo") {
  is_null(year)
  is_write_msg(msg = "fetch")
  ## break msg before showing message from cast_geo
  cat("..\n")

  ## if (is.null(table)){
  ##   yr <- as.integer(format(Sys.Date(), "%Y"))
  ##   table <- paste0("tblGeo", yr)
  ## }

  geoFile <- is_path_db(getOption("orgdata.geo"), check = TRUE)
  geo <- KHelse$new(geoFile)
  on.exit(geo$db_close(), add = TRUE)

  geo$tblvalue <- norgeo::cast_geo(year = year)
  geo$tblname <- table

  write <- is_write(write, table, geo$dbconn)
  if (write) {
    is_write_msg(msg = "write")
    geo$db_write(write = write)
    message("Write table `", table, "` is completed in: \n", geoFile)
  }

  if (append) {
    is_write_msg(msg = "append")
    geo$db_write(append = append)
    message("Append data to `", table, "` is completed in: \n", geoFile)
  }

  invisible(geo$tblvalue)
}

#' @title Geographical Codes
#' @description Create a table of current year geographical codes against previous
#'  years geogprahical codes. This is used to recode the previous years codes to the
#'  current codes. Implementation of this function is base on [norgeo::track_change()]
#'  function.
#' @param type Type of regional granularity ie. enumeration area (grunnkrets)
#' @param from Starting year for the range period. Current year is the default if left empty
#' @param to End of year for the range period. Current year is the default if left empty
#' @inheritParams geo_level
#' @importFrom norgeo track_change
#' @export
geo_recode <- function(type = c("grunnkrets", "bydel", "kommune", "fylke"),
                       from = NULL,
                       to = NULL,
                       write = FALSE) {
  type <- match.arg(type)
  yr <- to
  if (is.null(to)) {
    yr <- as.integer(format(Sys.Date(), "%Y"))
  }
  tblName <- paste0(type, yr)

  is_write_msg(msg = "fetch")

  ## Conn ----------------
  geoFile <- is_path_db(getOption("orgdata.geo"), check = TRUE)
  geo <- KHelse$new(geoFile)
  on.exit(geo$db_close(), add = TRUE)

  cat("..")
  if (type == "grunnkrets"){
    dtGrunn <- norgeo::track_change(type = type, from = from, to = to)
    geo$tblvalue <- is_unknown_manucipalty(dt = dtGrunn, from = from, to = to)
  } else                     {
    geo$tblvalue <- norgeo::track_change(type = type, from = from, to = to)
  }

  geo$tblname <- tblName

  write <- is_write(write, tblName, geo$dbconn)
  if (write) {
    is_write_msg(msg = "write")
    geo$db_write(write = write)
    message("Write table `", tblName, "` is completed in: \n", geoFile)
  }
  invisible(geo$tblvalue)
}


## Helper -----------------------------------------------------
## Warn user if table exists incase it's a mistake
is_write <- function(write, table, con) {
  tblExist <- DBI::dbExistsTable(conn = con, name = table)
  if (isTRUE(write) && isTRUE(tblExist)) {
    msgs <- sprintf("\nWoops!! Table `%s` allready exists. Do you want to overwrite?", table)
    ## write <- utils::askYesNo(msg = msgs, )
    yesNo <- utils::menu(c("Yes", "No"), title = msgs)
    write <- ifelse(yesNo == 1, TRUE, FALSE)
  }
  return(write)
}


is_write_msg <- function(msg = c("write", "append", "fetch")){
  msg <- match.arg(msg)
  switch(msg,
         write = message("\nStart writing data ..."),
         append = message("\nStart appending data ..."),
         fetch = cat("\nFetching data ...")
         )
}

## Ensure that all manucipality have unknown grunnkrets because
## sometime unknown grunnkrets doesn't exist in API
is_unknown_manucipalty <- function(dt, from, to){
  oldCode <- currentCode <- NULL

  kom <- norgeo::track_change("kommune", from = from, to = to)
  kom <- kom[!is.na(oldCode)]
  kom <- kom[, `:=`(oldCode = paste0(oldCode, "9999"),
                    currentCode = paste0(currentCode, "9999"))]

  codes <- setdiff(kom$oldCode, dt$oldCode)
  kom <- kom[oldCode %chin% codes, ]
  kom[, c("oldName", "newName") := "Uoppgitt"]

  data.table::rbindlist(list(dt, kom))
}
