#' @keywords internal
#' @title Process Raw Data
#' @description Implement the specifications to the raw data and deliver the output
#'   to the enclosing environment.
#' @param .env Enclosing environment
#' @return A dataset `.orgDT` with `data.table` format

is_org_process <- function(.env = parent.frame()) {
  dt <- read_file(file = .env$filePath)

  colSpec <- get_column_standard(spec = .env$fileSpec)
  dt <- do_column_standard(dt, colSpec)
  ## TODO Any extra args for file specific from INNLESARG

  splitSpec <- get_split(spec = .env$fgSpec)
  dt <- do_split(dt = dt, split = splitSpec)

  yrSpec <- get_year(.env$fileSpec, .env$kh$dbconn)
  dt <- do_year(dt, yrSpec)

  manSpec <- get_manheader(spec = .env$fileSpec)
  dt <- do_manheader(dt, manSpec)

  # output ----------------------------------
  assign(".orgDT", dt, envir = .env)
}
