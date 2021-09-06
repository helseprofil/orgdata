#' @title Show Website
#' @description Display website of `orgdata` package ie. \href{https://helseprofil.github.io/orgdata/}{https://helseprofil.github.io/orgdata/}
#' @param url The URL
#' @export
website <- function(url = NULL){
  if (is.null(url)){url = "https://helseprofil.github.io/orgdata"}
  utils::browseURL(url = url)
}
