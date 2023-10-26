
#' @title Update Global Options
#' @description Update global options based on the file in config repo
#' @export
update_globs <- function(){


  invisible()
}

#' @export
#' @rdname update_globs
up_opt <- update_globs

is_globs <- function(){
  optOrg <- yaml::yaml.load_file("https://raw.githubusercontent.com/helseprofil/config/main/config-orgdata.yml")
  as.list(opt_rename(optOrg))
}

#' @title Show Website
#' @description Display website of `orgdata` package ie. \href{https://helseprofil.github.io/orgdata/}{https://helseprofil.github.io/orgdata/}
#' @param url The URL
#' @export
website <- function(url = NULL){
  if (is.null(url))
    url = "https://helseprofil.github.io/orgdata"

  utils::browseURL(url = url)
}


#' @title Reset options
#' @description Reset orgdata options to default
#' @examples reset_options()
#' @export
reset_options <- function(){
  options(opt.orgdata)

  if (exists("orgEnv"))
    orgEnv$status <- 1
}

#' @title Status workflow
#' @description Status workflow to display message. Default is 1
orgEnv <- new.env()
orgEnv$status <- 1


#' @export
#' @rdname reset_options
reset_opt <- reset_options


#' @title Debugging options
#' @description A wrapper for debugging options.
#' @param opt Debug options
#' @param val Value options
#' @details Options available:
#'   - "shallow"   : Show the executed funcitons in the process flow
#'   - "deep"      : Show deeper including helper functions in the process flow
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
debug_opt <- function(opt = c("shallow", "deep", "nrow", "row", "aggregate", "geo"), val = NULL){
  opt <- match.arg(opt)
  switch(opt,
         shallow = options(orgdata.debug = TRUE),
         deep = options(orgdata.debug = "deep"),
         nrow = options(orgdata.debug.nrow = ifelse(is.null(val), 20, val)),
         row = options(orgdata.debug.row = ifelse(is.null(val), 1:50, val)),
         aggregate = options(orgdata.debug.aggregate = TRUE),
         geo = options(orgdata.debug.geo = TRUE))
}


#' @title Emoji
#' @description Change emoji in the output messages for fun &#128516;
#' @param x Emoji to choose ie. thumb, smile etc
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

  invisible()
}


#' @title Update package
#' @description Update orgdata directly with Github repo. Default is `main` branch.
#' @param branch The branch in Github to install from
#' @param force Use the laterst version(s) of all dependencies. Default is FALSE
#' @examples
#' \dontrun{
#' update_orgdata(branch = "main") #default
#' update_orgdata(branch = "dev") #to upgrade with dev branch
#' }
#' @export
update_orgdata <- function(branch = c("main", "dev"), force = FALSE){
  branch <- match.arg(branch)
  unloadNamespace("orgdata")
  switch(branch,
         main = pak::pkg_install("helseprofil/orgdata", upgrade = force),
         dev = pak::pkg_install("helseprofil/orgdata@dev", upgrade = force)
         )
  attachNamespace("orgdata")
  invisible()
}

#' @export
#' @rdname update_orgdata
upgrade_orgdata <- update_orgdata


## TESTTHAT ----------------
## Skip when no database file are found eg. in CRAN or CI
skip_error_db <- function(){

  dbFile <- fs::file_exists(is_path_db(getOption("orgdata.db")))

  if (isFALSE(dbFile))
    return(invisible(TRUE))

  testthat::skip("DB not found")
}

## Skip test when running R CMD check
skip_if_check <- function(){
  if (identical(Sys.getenv("ORGDATA_TEST", unset = "TRUE"), "TRUE"))
    return(invisible(TRUE))

  testthat::skip("Not run when CMD check")
}

## OPTIONS --------------
# Driver on different operating system
os_drive <- function(os = OS){
  switch(os,
         Linux = getOption("orgdata.linux.drive"),
         Windows = getOption("orgdata.win.drive"))
}

# add prefix "kh" to options
opt_rename <- function(x){
  x2 <- paste("orgdata", names(x), sep = ".")
  x <- stats::setNames(x, x2)
}

# Debug options -------
# Check if any debug options is activated
# This is usefull to deactivate KONTROLLERT and read raw file instead of from DuckDB
is_option_active <- function(){
  any(c(getOption("orgdata.debug.aggregate"),
        getOption("orgdata.debug.geo"),
        getOption("orgdata.read.raw")))
}

# Check version
is_latest_version <- function(ver = utils::packageDescription("orgdata")[["Version"]]){

  V1 <- V2 <- NULL
  out <- FALSE
  desc <- "https://raw.githubusercontent.com/helseprofil/orgdata/main/DESCRIPTION"

  isOn <- is_online(desc)

  if (isOn){
    gitDes <- data.table::fread("https://raw.githubusercontent.com/helseprofil/orgdata/main/DESCRIPTION", nrows = 4, fill = TRUE)
    gitVer <- gitDes[V1 %like% "Version", V2]

    newVer <- numeric_version(gitVer) > numeric_version(ver)

    if (newVer){
      is_color_txt(gitVer, "New version is available!", type = "note", emoji = TRUE, symbol = "thumb")
      is_color_txt(ver, "Your installed version:", type = "note", emoji = TRUE, symbol = "sad")
      is_color_txt("Changelog", "Find out what's new in", type = "note", emoji = TRUE, symbol = "book")
      out <- TRUE
    }
  } else {
    is_color_txt("", "You have no internet connection!",
                 type = "error", emoji = TRUE, symbol = "sad")
  }

  invisible(out)
}

is_online <- function(x){
  tryCatch({
    readLines(x, n=1)
    TRUE
  },
  error = function(e) FALSE,
  warning = function(w) tryInvokeRestart("muffleWarning")
  )
}
