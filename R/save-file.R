#' @title Save Data to CSV file
#' @description Save data as a `.csv` format file with semicolon `;` as seperation.
#' The file will be saved to the specified folder
#' as in `getOption("orgdata.folder.output")`. Use argument `save = TRUE`
#' in `read_raw()` will activate `save_file()` directly. Else you can call
#' `save_file()` to save the object output from `read_raw()`
#' @inheritParams do_split
#' @inheritParams read_raw
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

save_file <- function(dt = NULL, group = NULL){
  is_null(dt)
  is_null(group)

  file <- is_file_csv(group = group)
  data.table::fwrite(dt, file = file, sep = ";")
}

#' @export
#' @rdname save_file
lagfil <- save_file


## Helper -----------------------------------------

is_file_csv <- function(group, verbose = getOption("orgdata.verbose")){
  batch <- format(Sys.time(), format = "%Y%m%d_%H%M%S")
  fileName <- paste0(group, "_", batch, ".csv")
  fileOut <- file.path(getOption("orgdata.folder.output"), fileName)

  if (verbose){
    message("Save file: ", fileOut)
  }
  return(fileOut)
}
