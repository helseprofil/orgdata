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

  dotDot <- tryCatch(
    unlist(list(...)),
    error = function(err){err}
  )

  if (is(dotDot, "error")){
    dots <- deparse(substitute(list(...)))
    grp <- gsub(".*\\((.*)\\)", "\\1", dots)
    fgp <- unlist(strsplit(grp, split = ","))
  } else {
    fgp <- dotDot
  }

  for (i in fgp){
    i <- trimws(i)
    make_file(i, save = TRUE)
  }
}

#' @export
#' @rdname make_filegroups
lag_filgrupper <- make_filegroups
