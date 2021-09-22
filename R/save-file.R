#' @title Save Data to CSV file
#' @description Save data as a `.csv` format file with semicolon `;` as seperation.
#' The file will be saved to the specified folder
#' as in `getOption("orgdata.folder.output")`. Use argument `save = TRUE`
#' in `read_raw()` will activate `save_file()` directly. Else you can call
#' `save_file()` to save the object output from `read_raw()`
#' @inheritParams do_split
#' @inheritParams read_raw
#' @param date Output file will be named with date and time
#' @examples
#' \dontrun{
#'  opitions(orgdata.aggregate = TRUE)
#'  DF <- read_raw("BEFOLKNING", save = TRUE)
#'
#'  # Two steps
#'  DF <- read_raw("BEFOLKNING")
#'  save_file(DF, "BEFOLKNING")
#' }
#' @export

save_file <- function(dt = NULL, group = NULL, date = FALSE){
  is_null(dt)
  is_null(group)

  file <- is_file_csv(group = group, date = date)
  data.table::fwrite(dt, file = file, sep = ";")
}

#' @export
#' @rdname save_file
lagrefil <- save_file


## Helper -----------------------------------------

is_file_csv <- function(group, verbose = getOption("orgdata.verbose"), date){

  if (date){
    batch <- format(Sys.time(), format = "%Y%m%d_%H%M%S")
    fileName <- paste0(group, "_", batch, ".csv")
  } else {
    fileName <- paste0(group, ".csv")
  }

  fileOut <- file.path(getOption("orgdata.folder.output"), fileName)

  if (verbose){
    message("Save file: ", fileOut)
  }
  return(fileOut)
}
