#' @title Make Multiple Filegroups
#' @description Implement the specification in registration database on several
#'   filegroups at once. This function will only be used when all selected
#'   filegroups have been controlled for any errors. The selected filegroups
#'   will be saved as a `.csv` file with respective filegroup names and folder
#'   as in registration database.
#' @param ... Filegroup(s)
#' @examples
#' \dontrun{
#' make_filegroup(NEET, TRANGBODD, DODE)
#' }
#' @family filegroups functions
#' @export
make_filegroups <- function(...){

  fgp <- tryCatch({
    unlist(list(...))
  },
  error = function(err){err}
  )

  if (is(fgp, "error")){
    dots <- eval(substitute(alist(...)))
    fgp <- sapply(as.list(dots), deparse)
  }

  fgpKO <- list()
  fgpOK <- list()

  for (i in fgp){
    i <- trimws(i)

    FGP <- tryCatch({make_file(i, save = TRUE)},
                    error = function(err) err)

    if (is(FGP, "error")){
      fgpKO[i] <- i
      next
    } else {
      fgpOK[i] <- i
    }
  }

  is_line_short()
  msgOK <- paste0("Done ", length(fgpOK), " group(s):")
  is_color_txt(fgpOK, msg = msgOK, type = "note")
  is_color_txt(x = "`log$ok`", msg = "Check all the filegroups with:")

  if (length(fgpKO) > 0) {
    is_line_short()
    msgKO <- paste0("Error ", length(fgpKO), " group(s):")
    is_color_txt(fgpKO, msg = msgKO, type = "error")
    is_color_txt(x = "`log$ko`", msg = "Check all the filegroups with:")
  }
}

#' @export
#' @rdname make_filegroups
lag_filgrupper <- make_filegroups
