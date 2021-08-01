#' Read and Run the Specifications
#' @description
#' Read the specifications in the register database and implement them
#' to the selected group of files (\emph{filgruppe}). All files under
#' the selected group will be considered unless the \code{FILID} with
#' argument \code{id} is specified. Specifying \code{id} is useful for testing.
#'
#' The function [lesorg()] is an
#' alias to [read_org()].
#' @examples
#' \dontrun{
#' read_org("BEFOLKNING")
#' read_org("BEFOLKNING", id = 19)
#' read_org("BEFOLKNING", id = c(15, 50))
#' }
#' @param group The group of files (\emph{filgruppe})
#' @inheritParams find_year
#' @aliases read_org lesorg
#' @export
read_org <- function(group = NULL, id = NULL) {
  dbFile <- file.path(
    osDrive,
    getOption("orgdata.folder"),
    getOption("orgdata.file")
  )

  kh <- KHelse$new(dbFile)

  spec <- find_spec(
    file = "specification.sql",
    value = group,
    con = kh$dbconn
  )

  if (!is.null(id)) {
    ## TODO Select the id
  }

  ## TODO - Read line by line if it's more than 1 line
  odFiles <- nrow(spec)

  message(group, " has ", odFiles, " files.")

  ## for (i in seq_len(odFiles)){
  spec

  ## }
}


#' @export
#' @rdname read_org
lesorg <- read_org
