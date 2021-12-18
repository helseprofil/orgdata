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
emoji <- function(x = c("thumb", "smile", "sad")){
  x <- match.arg(x)
  switch(x,
         thumb = options(orgdata.emoji = "thumb"),
         smile = options(orgdata.emoji = "smile"),
         sad = options(orgdata.emoji = "sad")
         )
}
