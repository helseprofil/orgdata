#' Read and Run the Specification
#' @description
#' Read the specifications in the register database and implement them
#' to the selected group of files (\emph{filgruppe}). All files under
#' the selected group will be considered unless the \code{FILID} with
#' argument \code{id} is specified. The function [lesorg()] is an
#' alternative function to [read_org()].
#' @param group The group of files (\emph{filgruppe})
#' @inheritParams get_year_from_file
#' @export
read_org <- function(group = NULL, id = NULL) {
  dbFile <- file.path(
    osDrive,
    getOption("orgdata.folder"),
    getOption("orgdata.file")
  )

  kh <- KHelse$new(dbFile)

  spec <- read_spec(
    file = "specification.sql",
    value = group,
    con = kh$dbconn
  )

  ## TODO - Read line by line if it's more than 1 line
  odFiles <- nrow(spec)
}


#' @export
#' @rdname read_org
lesorg <- function(...) {
  read_org(...)
}
