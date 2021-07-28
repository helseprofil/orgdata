#' Read and Run the Specifications
#' @description
#' Read the specifications in the register database and implement them
#' to the selected group of files (\emph{filgruppe}). All files under
#' the selected group will be considered unless the \code{FILID} with
#' argument \code{id} is specified. This is useful for testing.
#'
#' The function [lesorg()] is an
#' alias to [read_org()].
#' @param group The group of files (\emph{filgruppe})
#' @inheritParams find_year
#' @aliases lesorg
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

  ## TODO - Read line by line if it's more than 1 line
  odFiles <- nrow(spec)

  message(group, " has ", odFiles, " files.")

  ## for (i in seq_len(odFiles)){


  ## }
}


#' @export
#' @rdname read_org
lesorg <- read_org
