#' @title Orgdata logo
#' @description Display orgdata logo
#' @return Orgdata logo
#' @examples
#' \dontrun{
#' if(interactive()){
#'  print.orgdata_logo()
#'  }
#' }
#' @rdname orgdata_logo
#' @keywords internal
#' @importFrom cli col_br_magenta
orgdata_logo <- function() {

  logo <- c(
    "\n ",
    "\n                   |     |         ",
    "\n,---.,---.,---.,---|,---.|--  ,---.",
    "\n|   ||    |   ||   |,---||    ,---|",
    "\n`---'`    `---|`---'`---^`---'`---^",
    "\n          `---'                    "
    )

  structure(cli::col_br_magenta(logo), class = "orgdata_logo")
}


#' @export
print.orgdata_logo <- function(x, ...) {
  cat(x, ..., sep = "\n")
  invisible(x)
}
