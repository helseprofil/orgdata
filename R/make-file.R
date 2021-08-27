#' Aggregate Rawdata Asis
#' @description Aggregate rawdata as it is ie. keeping the columnames unchanged.
#' @inheritParams read_file
#' @inheritParams do_aggregate
#' @keywords internal
make_raw <- function(file, geo, val){
  options(orgdata.active = FALSE)
  dt <- read_file(file = file)
  dt <- do_aggregate(dt, geo = geo, val = val)
  on.exit(options(orgdata.active = TRUE))
  invisible(dt)
}


file_init <- function(
                      file = NULL,
                      geo = NULL,
                      val = NULL,
                      split = NULL){

}
