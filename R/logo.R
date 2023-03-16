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
#' @export
#' @importFrom cli col_black
orgdata_logo <- function() {

  logo <- c(
    " ",
    "                   |     |         ",
    ",---.,---.,---.,---|,---.|--- ,---.",
    "|   ||    |   ||   |,---||    ,---|",
    "`---'`    `---|`---'`---^`---'`---^",
    "          `---'                    ",
    " "
    )

  structure(cli::col_black(logo), class = "orgdata_logo")
}


#' @export
print.orgdata_logo <- function(x, ...) {
  cat(x, ..., sep = "\n")
  invisible(x)
}
