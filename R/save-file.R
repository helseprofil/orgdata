#' @title Save Data to CSV file
#' @description Save data as a `.csv` format file with semicolon `;` as seperation.
#' The file will be saved to the specified folder in `path` or as in Access regstration database
#' with the root as in `getOption("orgdata.folder.data")`. Use argument `save = TRUE`
#' in `make_file()` will activate `save_file()` directly. Else you can call
#' `save_file()` to save the object output from `make_file()`
#' @inheritParams do_split
#' @inheritParams make_file
#' @param path Folder path to save the file. If not specified then the path
#'  must to be specified in Access registration database
#' @param date Output file will be named with date and time
#' @param fgSpec File group specification from Access registration database
#' @examples
#' \dontrun{
#'  # Save file directly
#'  make_file("BEFOLKNING", save = TRUE)
#'
#'  # Two steps
#'  DF <- make_file("BEFOLKNING")
#'  save_file(DF, "BEFOLKNING")
#' }
#' @export

save_file <- function(dt = NULL,
                      group = NULL,
                      path = NULL,
                      date = FALSE,
                      fgSpec = NULL){
  is_null(dt)
  is_null(group)

  file <- is_file_csv(group = group, path = path, date = date, fgSpec = fgSpec)
  data.table::fwrite(dt, file = file, sep = ";")
}

#' @export
#' @rdname save_file
lagre_fil <- save_file


## Helper -----------------------------------------

is_file_csv <- function(group, path, date, verbose = getOption("orgdata.verbose"), fgSpec){

  if (date){
    batch <- format(Sys.time(), format = "%Y%m%d_%H%M%S")
    fileName <- paste0(group, "_", batch, ".csv")
  } else {
    fileName <- paste0(group, ".csv")
  }

  if (is.null(path)){
    fpath <- is_save_path(group = group, fgSpec = fgSpec)
    fileOut <- file.path(fpath, fileName)
  } else {
    fileOut <- file.path(path, fileName)
    if (!fs::dir_exists(path)) {stop(simpleError(message = "Folder not found!", call = path))}
  }

  if (verbose){
    message("Save file: ", fileOut)
  }
  invisible(fileOut)
}

is_save_path <- function(group = NULL, fgSpec = NULL){

  if (is.null(fgSpec)){
    dbFile <- is_path_db(
      db = getOption("orgdata.db"),
      check = TRUE
    )

    ## CONNECTION--------------------------------------
    kh <- is_conn_db(dbFile)

    ## SPECS -----------------------------------------
    fgSpec <- find_spec(
      file = "filegroups.sql",
      value = group,
      con = kh$dbconn
    )
  }

  folder <- fgSpec$UTMAPPE
  fullPath <- file.path(getOption("orgdata.folder.data"), folder)
  ## fullPath <- normalizePath(fullPath, winslash = "/")

  if (!fs::dir_exists(fullPath)) {
    is_verbose(x = fullPath, msg = "New folder is created:")
    fs::dir_create(fullPath)
  }

  invisible(fullPath)
}
