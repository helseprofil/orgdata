#' @title Granularity of Geographical Codes
#' @description Create a database granularity of geographical codes to aggregate
#'   data accordingly. Implementation of this function is base on [norgeo::cast_geo()]
#'   function in \href{https://helseprofil.github.io/norgeo/}{norgeo} package.
#' @param year Year for the valid geographical codes
#' @param write Write table to the `orgdata.geo` database. It will overwrite
#'    the table if it already exists
#' @param append Append the data to an existing table in the `orgdata.geo`
#' @param table Table name to be created in the database. Default is `tblGeo`
#' @importFrom norgeo cast_geo
#' @family geo codes functions
#' @examples
#' \dontrun{
#' geo_map(2020, write = TRUE)
#' geo_map(2021, append = TRUE)
#' }
#' @export
geo_map <- function(year = NULL, write = FALSE, append = FALSE, table = "tblGeo") {
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
  DT <- is_grunnkrets_00(DT)
  geo$tblvalue <- DT[, "batch" := is_batch("date")]
  geo$tblname <- table

  is_write(write, table, geo$dbconn)

  if (write) {
    is_write_msg(msg = "write")
    geo$db_write(write = write)
    msgWrite <- paste0("Write table `", table, "` is completed in: \n")
    is_verbose(x = geoFile, msg = msgWrite, type = "note")
  }

  if (append) {
    is_write_msg(msg = "append")
    geo$db_write(append = append)
    msgAppend <- paste0("Append data to `", table, "` is completed in: \n")
    is_verbose(x = geoFile, msg = msgAppend, type = "note")
  }

  return(geo$tblvalue)
}

#' @title Geographical Codes to Recode
#' @description Create a table of current year geographical codes against previous
#'  years geogprahical codes. This is used to recode the previous years codes to the
#'  current codes. Implementation of this function is base on [norgeo::track_change()]
#'  function.
#' @param type Type of regional granularity ie. enumeration area (grunnkrets)
#' @param from Starting year for the range period. Current year is the default if left empty
#' @param to End of year for the range period. Current year is the default if left empty
#' @inheritParams geo_map
#' @importFrom norgeo track_change
#' @family geo codes functions
#' @examples
#' \dontrun{
#' geo_recode(type = "grunnkrets", from = 2018, to = 2021, write = TRUE)
#' geo_recode(type = "grunnkrets", from = 2018, to = 2021, append = TRUE)
#' }
#' @export
geo_recode <- function(type = c("grunnkrets", "bydel", "kommune", "fylke"),
                       from = NULL,
                       to = NULL,
                       write = FALSE,
                       append = FALSE) {

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
    dtGrunn <- get_grunnkrets_dummy(dt = dtGrunn, from = from, to = to)
    geo$tblvalue <- dtGrunn[, "batch" := is_batch("date")]
  } else                     {
    dtLevels <- norgeo::track_change(type = type, from = from, to = to)
    geo$tblvalue <- dtLevels[, "batch" := is_batch("date")]
  }

  geo$tblname <- tblName

  is_write(write, tblName, geo$dbconn)

  if (write) {
    is_write_msg(msg = "write")
    geo$db_write(write = write)
    msgWrite <- paste0("Write table `", tblName, "` is completed in: \n")
    is_verbose(x = geoFile, msg = msgWrite, type = "note")
  }

  if (append) {
    is_write_msg(msg = "append")
    geo$db_write(append = append)
    msgAppend <- paste0("Append data to `", tblName, "` is completed in: \n")
    is_verbose(x = geoFile, msg = msgAppend, type = "note")
  }

  return(geo$tblvalue)
}


#' @title Merge Other Geo Level Manually
#' @description Geo codes other than those downloaded from SSB API can be merged
#'   to the main geo table ie. `tblGeo` in the geocodes database. The file must
#'   consist of id column to merge into ie. `id.file` and the geo codes to add
#'   to ie. `geo.col`.
#' @param id.table ID columname to merge to that is found in the database eg.
#'   `kommune`
#' @param id.file ID columname from the file to merge from. This depends on the
#'   columnames in the files. If `id.table` is `kommune`, then `id.file` must be
#'   the columname representing geo codes that is equivalent to `kommune` codes.
#'   Both `id.table` and `id.file` will be used for merging and these codes must
#'   be unique.
#' @param geo.col Columname where the new geo codes are eg. if the new geo codes
#'   is on levekaar, then `geo.col` is the columname where codes for levekaar
#'   can be found.
#' @param geo.level Geographical level of the merged file will be representing
#'   eg. "levekaar". This will be the value in column `level` in the `tblGeo` in
#'   the database.
#' @param file Complete path of filename to merge from
#' @param year Year the code is valid for. If not sepecified `orgdata.year` is
#'   used.
#' @inheritParams geo_map
#' @param table.name Name of the table for geo recode in geocodes database. This
#'   can be found with `getOptions("orgdata.geo")`. The default is `tblGeo`.
#' @param ... Other possible arguments
#' @examples
#' \dontrun{
#' dt <- geo_merge(id.table = "grunnkrets",
#'                 id.file = "id",
#'                 geo.col = "col2",
#'                 geo.level = "levekaar",
#'                 file = "C:/path/to/file.csv",
#'                 year = 2022)
#' }
#' @export

geo_merge <- function(id.table = NULL,
                      id.file = NULL,
                      geo.col = NULL,
                      geo.level = NULL,
                      file = NULL,
                      year = NULL,
                      write = FALSE,
                      table.name = "tblGeo", ...){

  code <- level <- name <- validTo <- NULL

  # when testing, use the file in dev folder
  file <- test_file(file = file, ...)

  is_null(id.table, verbose = FALSE)
  is_null(id.file, verbose = FALSE)
  is_null(arg = file, verbose = FALSE)
  is_null(arg = geo.level, verbose = FALSE)

  geoDB <- is_path_db(getOption("orgdata.geo"), check = TRUE)
  geo <- KHelse$new(geoDB)
  on.exit(geo$db_close(), add = TRUE)

  DT <- geo$db_read(table.name)
  dt <- read_file(file, encoding = "UTF-8", colClasses = "character")

  ## dupID <- dt[duplicated(get(geo.col))][[1]]
  ## dt <- dt[!is.na(get(geo.col))]
  ## dt <- dt[!duplicated(get(geo.col))]

  delCols <- setdiff(names(dt), c(id.file, geo.col))
  dt[, (delCols) := NULL]

  is_unique_id(dt = dt, id = id.file)

  data.table::setkeyv(DT, id.table)
  data.table::setkeyv(dt, id.file)

  ## Dataset for selected geo.level
  geoCols <- names(DT)
  dd <- data.table::copy(dt)
  dd[DT, (geoCols) := mget(geoCols)]
  dd[, level := geo.level]
  dd[, name := NA]
  dd[, code := get(id.file)]
  if (is.null(year)){
    year <- getOption("orgdata.year")
  }
  dd[, validTo := year]

  if (isFALSE(id.table == id.file)){
    dd[ , (id.file) := NULL]
  }

  # Dataset merged to main table
  DT[dt, (geo.col) := get(geo.col)]
  data.table::setnames(DT, geo.col, geo.level)

  DT <- data.table::rbindlist(list(DT, dd), use.names = TRUE)
  data.table::setcolorder(DT, names(DT)[names(DT)!= "batch"])
  data.table::setkey(DT, code)

  if (isFALSE(write)){
    write <- utils::askYesNo("Should the result be added to geo database?", default = FALSE)
  }

  if (isTRUE(write)) {
    is_write_msg(msg = "write")
    msgWrite = "This will take time to complete ..."
    withr::with_options(list(orgdata.emoji = "sad"),
                        is_colour_txt(x = "", msgWrite, type = "note", emoji = TRUE))
    geo$db_write(name = table.name, value = DT, write = write)
    msgWrite <- paste0("Write table `", table.name, "` is completed in: \n")
    is_verbose(x = geoDB, msg = msgWrite, type = "note")
  }

  return(DT)
}

#' @title Create Dummy Enumeration Area Codes
#' @description Some of the downloaded enumeration area codes from SSB lack
#'   codes for missing ie. `xxxx9999`. This function create these codes that
#'   are needed for recoding older codes to current enumeration area codes.
#' @param dt Downloaded data with norgeo::track_change()
#' @inheritParams geo_recode
#' @family geo codes functions
#' @export
get_grunnkrets_dummy <- function(dt, from, to){
  kommune <- norgeo::track_change("kommune", from = from, to = to)
  dt <- is_unknown_grunnkrets(dt, kommune)
  dt <- is_grunnkrets_99(dt)
}

## Helper -----------------------------------------------------
## Warn user if table exists incase it's a mistake
is_write <- function(write, table, con, answer = 0) {
  tblExist <- DBI::dbExistsTable(conn = con, name = table)
  if (isTRUE(write) && isTRUE(tblExist)) {
    msgs <- sprintf("\nWoops!! Table `%s` allready exists. What will you do?", table)
    ## write <- utils::askYesNo(msg = msgs, )
    answer <- utils::menu(c("Overwrite", "Append", "Cancel"), title = msgs)
  }

  if (answer == 1){
    is_assign_var("write", TRUE)
    is_assign_var("append", FALSE)
  }

  if (answer == 2){
    is_assign_var("write", FALSE)
    is_assign_var("append", TRUE)
  }

  if (answer == 3){
    is_assign_var("write", FALSE)
    is_assign_var("append", FALSE)
  }

  invisible()
}

is_assign_var <- function(var, val){
  assign(var, val, envir = sys.frames()[[1]])
}


is_write_msg <- function(msg = c("write", "append", "fetch")){
  msg <- match.arg(msg)
  switch(msg,
         write = message("Start writing data ..."),
         append = message("Start appending data ..."),
         fetch = cat("Fetching data ...")
         )
}


## Issue #39
## Ensure that all manucipality have unknown grunnkrets because
## sometime unknown grunnkrets doesn't exist in API
is_unknown_grunnkrets <- function(dt, kom){
  # kom - municipality data from norgeo::track_change()
  oldCode <- currentCode <- NULL

  kom <- data.table::copy(kom)

  kom <- kom[!is.na(oldCode)]
  kom <- kom[, `:=`(oldCode = paste0(oldCode, "9999"),
                    currentCode = paste0(currentCode, "9999"))]

  dt <- data.table::rbindlist(list(dt, kom))
  data.table::setkey(dt, currentCode)
}


## Grunnkrets that ends with 00 has no corresponds code from API. Needs to do it
## manually by comparing the second line of grunnkrets code. If grunnkrets code
## ends with 00 exists but has missing for kommune, fylke and bydel then add the
## code to these geo levels accordingly.
is_grunnkrets_00 <- function(dt){
  code <- level <- NULL
  is_verbose(msg = "Searching for geo level with NA due to grunnkrets code ends with 00")

  dt <- copy(dt)
  data.table::setkey(dt, code)
  idx <- dt[, .I[level == "grunnkrets" & code %like% "00$"]]
  levels <- c("kommune", "fylke","bydel")

  #cat(".") for every 40th index number
  cax <- idx[seq(1, length(idx), 40)]

  ## pb <- txtProgressBar(min = 0, max = length(idx), style = 3)

  for (i in idx){
    ## setTxtProgressBar(pb, i)
    ixrange <- c(i, i + 1)
    code01 <- sub("\\d{2}$", "", dt[ixrange[1], code])
    code02 <- sub("\\d{2}$", "", dt[ixrange[2], code])
    ## Similar codes indicate first and second line are of the same kommune
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


## To avoid error that recode not found
is_grunnkrets_99 <- function(dt){
  gr99 <- is.element("99999999", dt$oldCode)
  yrs <- as.integer(unique(dt$changeOccurred))

  if (isFALSE(gr99)){
    dt99 <- data.table::data.table(changeOccurred = max(yrs))
    dt99[, c("oldCode", "currentCode") := "99999999"]
    dt99[, c("oldName", "newName") := "Uoppgitt"]
    dt <- data.table::rbindlist(list(dt, dt99), use.names = TRUE)
  }

  return(dt)
}

test_file <- function(file = NULL, .test = FALSE){
  if(.test){
    file <- file.path(here::here(), "dev/levekaar.csv")
  }

  return(file)
}

is_unique_id <- function(dt, id){
  dupID <- dt[duplicated(get( id ))][[id]]
  if (length(dupID) > 0){
    msg <- paste0("Column `", id, "` must be unique to be able to merge!")
    is_color_txt(x = "", msg = msg, type = "error")
    is_stop("Found duplicated `id.file`:", is_short_code(dupID, n2 = 8))
  }
  invisible()
}
