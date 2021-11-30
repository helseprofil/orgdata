#' @title Make Multiple Filegroups
#' @description Implement the specification in registration database on several
#'   filegroups at once. This function will be only used when all selected
#'   filegroups have been controlled for any errors. The selected filegroups
#'   will be save as a `.csv`.
#' @param ... Filegroup(s)
#' @examples
#' \dontrun{
#' make_filegroup(NEET, TRANGBODD, DODE)
#' }
#' @family filegroups functions
#' @export
make_filegroup <- function(...){

  quo <- tryCatch(
    unlist(list(...)),
    error = function(err){err}
  )

  if (is(quo, "error")){
    dots <- deparse(substitute(list(...)))
    grp <- gsub(".*\\((.*)\\)", "\\1", dots)
    fgp <- unlist(strsplit(grp, split = ","))
  } else {
    fgp <- quo
  }

  for (i in fgp){
    i <- trimws(i)
    make_file(i, save = TRUE)
  }
}

#' @export
#' @rdname make_filegroup
lag_filgruppe <- make_filegroup
