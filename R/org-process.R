#' @keywords internal
#' @title Process Raw Data
#' @description Implement the specifications to the raw data and deliver the output
#'   to the enclosing environment.
#' @param file File rawdata
#' @param filespec Specification for a file from `tbl_Innlesing`
#' @param fgspec Specification for a file group
#' @param con Connection to the database
#' @param verbose Make processes explicit. Default is FALSE
#' @return A dataset with `data.table` format

is_org_process <- function(file, filespec, fgspec, con, verbose = getOption("orgdata.verbose")) {
  dt <- read_file(file = file)

  colSpec <- get_column_standard(spec = filespec)
  dt <- do_column_standard(dt, colSpec)
  ## TODO Any extra args for file specific from INNLESARG

  splitSpec <- get_split(spec = fgspec)
  dt <- do_split(dt = dt, split = splitSpec)

  yrSpec <- get_year(filespec, con)
  dt <- do_year(dt, yrSpec)

  manSpec <- get_manheader(spec = filespec)
  dt <- do_manheader(dt, manSpec)

}
