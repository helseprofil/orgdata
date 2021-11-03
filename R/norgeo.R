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

  ## -----------------------------------------
  ## Note: No need to create table with year eg. tblGeo2021
  ## since column ValidTo will be used to select valid year to aggregate
  ## -----------------------------------------

  geoFile <- is_path_db(getOption("orgdata.geo"), check = TRUE)
  geo <- KHelse$new(geoFile)
  on.exit(geo$db_close(), add = TRUE)

  DT <- norgeo::cast_geo(year = year)
  geo$tblvalue <- is_grunnkrets_00(DT)
  geo$tblname <- table

  write <- is_write(write, table, geo$dbconn)
  if (write) {
    is_write_msg(msg = "write")
    geo$db_write(write = write)
    msgWrite <- paste0("Write table `", table, "` is completed in: \n")
    is_colour_txt(x = geoFile, msg = msgWrite, type = "note")
    ## message("Write table `", table, "` is completed in: \n", geoFile)
  }

  if (append) {
    is_write_msg(msg = "append")
    geo$db_write(append = append)
    msgAppend <- paste0("Append data to `", table, "` is completed in: \n")
    is_colour_txt(x = geoFile, msg = msgAppend, type = "note")
    ## message("Append data to `", table, "` is completed in: \n", geoFile)
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
    msgWrite <- paste0("Write table `", tblName, "` is completed in: \n")
    is_colour_txt(x = geoFile, msg = msgWrite, type = "note")
    ## message("Write table `", tblName, "` is completed in: \n", geoFile)
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

  dt <- data.table::rbindlist(list(dt, kom))
  data.table::setkey(dt, currentCode)
}


## Grunnkrets that ends with 00 has no corresponds code from API
## Needs to do it manually comparing the second line with grunnkrets
## ends with 00 exist and has missing other geo levels
is_grunnkrets_00 <- function(dt){
  code <- level <- NULL
  is_verbose(msg = "Searching geo level with NA due to grunnkrets end with 00 ....")

  dt <- copy(dt)
  data.table::setkey(dt, code)
  idx <- dt[, .I[level == "grunnkrets" & code %like% "00$"]]
  levels <- c("kommune", "fylke","bydel")

  cax <- idx[seq(1, length(idx), 40)]
  ## pb <- txtProgressBar(min = 0, max = length(idx), style = 3)

  for (i in idx){
    ## setTxtProgressBar(pb, i)
    ixrange <- c(i, i + 1)
    code01 <- sub("\\d{2}$", "", dt[ixrange[1], code])
    code02 <- sub("\\d{2}$", "", dt[ixrange[2], code])

    same <- identical(code01, code02)

    if (same){
      dtlike <- dt[ixrange]
      dt <- is_delete_index(dt, ixrange)
      for (j in levels) {
        data.table::set(dtlike, which(is.na(dtlike[[j]])), j = j, value = dtlike[2, get(j)])
      }
      dt <- data.table::rbindlist(list(dt, dtlike))
      data.table::setkey(dt, code)
    }

    if (is.element(i, cax)) cat(".")
  }

  cat("\n")
  return(dt)
}
