#' @title Show Website
#' @description Display website of `orgdata` package ie. \href{https://helseprofil.github.io/orgdata/}{https://helseprofil.github.io/orgdata/}
#' @param url The URL
#' @export
website <- function(url = NULL){
  if (is.null(url)){url = "https://helseprofil.github.io/orgdata"}
  utils::browseURL(url = url)
}


#' @title Reset options
#' @description Reset orgdata options to default
#' @examples reset_options()
#' @export
reset_options <- function(){
  options(opt.orgdata)
}


#' @export
#' @rdname reset_options
reset_opt <- reset_options

#' @title Debugging options
#' @description A wrapper for debugging options.
#' @param opt Debug options
#' @param val Value options
#' @details Options available:
#'   - "fun"       : Activating options `orgdata.debug`
#'   - "nrow"      : For `orgdata.debug.nrow`. Default value is 20 if `val` argument is missing
#'   - "row"       : For `orgdata.debug.row`. Default value is 1:50 if `val` argument is missing
#'   - "aggregate" : Activating `orgdata.debug.aggregate`
#'   - "geo"       : Activating `orgdata.debug.geo`
#' @examples
#' \dontrun{
#' debug_opt("geo")
#' debug_opt("nrow", 30)
#' }
#' @export
debug_opt <- function(opt = c("fun", "nrow", "row", "aggregate", "geo"), val = NULL){
  opt <- match.arg(opt)
  switch(opt,
         fun = options(orgdata.debug = TRUE),
         nrow = options(orgdata.debug.nrow = ifelse(is.null(val), 20, val)),
         row = options(orgdata.debug.row = ifelse(is.null(val), 1:50, val)),
         aggregate = options(orgdata.debug.aggregate = TRUE),
         geo = options(orgdata.debug.geo = TRUE))
}


#' @title Emoji
#' @description Use emoji for fun :smile:
#' @param x Emoji to choose ie. thumb, smile or sad
#' @examples emoji("smile")
#' @export
emoji <- function(x = c("mark", "thumb", "write",
                        "smile", "sad", "santa",
                        "search", "folder", "book")){
  x <- match.arg(x)
  switch(x,
         mark = options(orgdata.emoji = "mark"),
         thumb = options(orgdata.emoji = "thumb"),
         write = options(orgdata.emoji = "write"),
         smile = options(orgdata.emoji = "smile"),
         sad = options(orgdata.emoji = "sad"),
         santa = options(orgdata.emoji = "santa"),
         search = options(orgdata.emoji = "search"),
         folder = options(orgdata.emoji = "folder"),
         book = options(orgdata.emoji = "book")
         )
}


## TESTTHAT ----------------
## Skip when no database file are found eg. in CRAN or CI
skip_error_db <- function(){

  dbFile <- fs::file_exists(is_path_db(getOption("orgdata.db")))

  if (isFALSE(dbFile)){
    return(invisible(TRUE))
  }

  testthat::skip("DB not found")
}

## Skip test when running R CMD check
skip_if_check <- function(){
  if (identical(Sys.getenv("ORGDATA_TEST", unset = "TRUE"), "TRUE")){
    return(invisible(TRUE))
  }

  testthat::skip("Not run when CMD check")
}
