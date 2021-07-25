# SQL code need sprintf for dynamic query
check_sql <- function(x) {
  if (grepl("%", x) != 1) {
    stop("Missing sprintf reference in SQL code")
  }
}


check_null <- function(arg) {
  argchr <- deparse(substitute(arg))
  if (is.null(arg)) {
    stop("Argument for ", argchr, " is missing")
  }
}
