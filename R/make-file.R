#' Aggregate Rawdata Asis
#' @description Aggregate rawdata as it is
#' @inheritParams read_file
#' @inheritParams do_aggregate
#' @export
make_file <- function(file, geo, val){
  op <- options()
  options(orgdata.active = FALSE)
  dt <- read_file(file = file)
  dt <- do_aggregate(dt, geo = geo, val = val)
  on.exit(options(op))
  invisible(dt)
}
