#' @title Save Data to CSV file
#' @description Save data as a `.csv` format file with semicolon `;` as
#'   seperator. The file will be saved to the specified folder in `path` or as
#'   in Access regstration database with the root as in
#'   `getOption("orgdata.folder.data")`. Use argument `save = TRUE` in
#'   `make_file()` will activate `save_file()` directly. Else you can call
#'   `save_file()` to save the object output from `make_file()`. This function
#'   is a wrapper to `data.table:fwrite()`.
#' @inheritParams do_split
#' @param name Filename for the `.csv` file or filegroup name
#' @param path Folder path to save the file. If `name` is a valide filegroup
#'   \emph{(FILGRUPPE)} then use the specified `UTMAPPE` in Access registration
#'   database else file will be saved in default folder
#'   `C:\Users\YourUserName\orgdata_logs`. The default folder will be created if
#'   not exist.
#' @param date Logical value. If TRUE then use date and time as part of the filename. Default is FALSE.
#' @param fgSpec File group specification from Access registration database
#' @param sep The separator between columns. Default is `";"`
#' @param ... Other arguments for `data.table::fwrite`
#' @examples
#' \dontrun{
#'  # Save file directly
#'  make_file("BEFOLKNING", save = TRUE)
#'
#'  # Two steps
#'  DF <- make_file("BEFOLKNING")
#'  save_file(DF, "BEFOLKNING")
#'
#'  # Save with different name and specified folder
#'  save_file(DF, name = "myCSVfile", path = "C:/MyFolder")
#' }
#' @export

save_file <- function(dt = NULL,
                      name = NULL,
                      path = NULL,
                      date = FALSE,
                      fgSpec = NULL,
                      sep = ";", ...){
  is_null(dt)
  is_null(name)

  file <- is_file_csv(group = name, path = path, date = date, fgSpec = fgSpec, action = "save")
  data.table::fwrite(dt, file = file, sep = sep, ...)
}

#' @export
#' @rdname save_file
lagre_fil <- save_file


## Helper -----------------------------------------
# action - To display message as save or read file
is_file_csv <- function(group = NULL,
                        path = NULL,
                        date = FALSE,
                        verbose = NULL,
                        fgSpec = NULL,
                        action = c("save", "read")){

  if (is.null(verbose)) verbose <- getOption("orgdata.verbose")

  if (date){
    batch <- is_batch("time")
    fileName <- paste0(group, "_", batch, ".csv")
  } else {
    fileName <- paste0(group, ".csv")
  }

  if (is.null(path)){
    fpath <- is_save_path(group = group, fgSpec = fgSpec, action = action)
    fileOut <- file.path(fpath, fileName)
  } else {
    fileOut <- file.path(path, fileName)
    if (!fs::dir_exists(path)) {
      is_stop(msg = "Folder not found!", var = path)
    }
  }

  msg <- switch(action,
                save = "Save file:",
                read = "Read file:",
                "File:")

  fileOut <- gsub("\\\\", "/", fileOut)

  if (action == "read"){
    withr::local_options(list(orgdata.verbose = FALSE))
  }

  is_verbose(fileOut, msg = msg)

  return(fileOut)
}

is_save_path <- function(group = NULL, fgSpec = NULL, ...){

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

  if (nrow(fgSpec) == 0){
    is_verbose("Invalid Filegroup or `path` is missing")
    outPath <- is_orgdata_path()
  } else {
    outPath <- is_group_path(fgSpec, ...)
  }

  return(outPath)
}

is_group_path <- function(fgSpec, action){

  folder <- fgSpec$UTMAPPE
  fullPath <- file.path(os_drive(), getOption("orgdata.folder.data"), folder)
  ## fullPath <- normalizePath(fullPath, winslash = "/")

  if (!fs::dir_exists(fullPath)) {
    if (action == "save"){
      is_verbose(x = fullPath, msg = "New folder is created:")
      fs::dir_create(fullPath)
    } else {
      is_stop(msg = "Folder not found!", var = fullPath)
    }
  }

  invisible(fullPath)
}

