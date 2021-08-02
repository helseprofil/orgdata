#' Read and Run the Specifications
#' @description
#' Read the specifications in the register database and implement them
#' to the selected group of files (\emph{filgruppe}). All files under
#' the selected group will be considered unless the \code{KOBLID} with
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
#' @param id \code{KOBLID} from table \emph{tbl_Koble}
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

  koblid <- spec$KOBLID
  if (!is.null(id)) {
    koblid <- id
    spec <- spec[spec$KOBLID %in% koblid, ]
  }

  message(group, " has ", length(koblid), " files.")

  DT <- vector(mode = "list", length = length(koblid))

  for (i in seq_len(length(koblid))) {
    filespec <- spec[i, ]
    filename <- find_column_input(filespec, "FILNAVN")
    filepath <- file.path(osDrive, getOption("orgdata.rawdata"), filename)
    dt <- read_file(filepath)
    DT[[i]] <- dt
  }

  out <- data.table::rbindlist(DT)
}


#' @export
#' @rdname read_org
lesorg <- read_org
