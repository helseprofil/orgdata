#' @title Orgdata logo
#' @description Display orgdata logo
#' @param unicode Use utf-8. Default: cli::is_utf8_output()
#' @return Orgdata logo
#' @examples
#' \dontrun{
#' if(interactive()){
#'  print.orgdata_logo()
#'  }
#' }
#' @seealso
#'  \code{\link[cli]{is_utf8_output}}, \code{\link[cli]{ansi-styles}}
#' @rdname orgdata_logo
#' @export
#' @importFrom cli is_utf8_output col_blue
orgdata_logo <- function(unicode = cli::is_utf8_output()) {

  logo <- c(
    " ",
    "                   |     |         ",
    ",---.,---.,---.,---|,---.|--- ,---.",
    "|   ||    |   ||   |,---||    ,---|",
    "`---'`    `---|`---'`---^`---'`---^",
    "          `---'                    "
  )

  structure(cli::col_blue(logo), class = "org_logo")
}


#' @export
print.orgdata_logo <- function(x, ...) {
  cat(x, ..., sep = "\n")
  invisible(x)
}
