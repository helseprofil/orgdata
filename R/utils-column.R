
#' Columns with Single Input
#' @description
#' Get the value of a column when it's only one input is allowed.
#' @param spec Specifications data as a data.frame
#' @param type Type of object as output ie. double, integer or character.
#'     Default is character.
#' @param col Column name in the database table
#' @family input-argument functions
#' @export
find_column_input <- function(spec = NULL, col = NULL, type = c("character", "double", "integer")) {
  # spec : Input data as data.frame
  # col : Selected column in spec
  # type : type of input object will be checked with typeof()
  type <- match.arg(type)
  val <- trimws(spec[, col])
  val <- is_input_type(val, type)
  val <- is_logical(val)
  return(val)
}


#' @keywords internal
#' @title Type of object input
#' @description Convert value to selected type ie. checked with [typeof]
#' @param value Input value to be converted
is_input_type <- function(value, type = c("character", "double", "integer")) {
  # value : value to be converted
  # type : type of input object will be checked with typeof()
  type <- match.arg(type)
  val <- switch(type,
    double = as.numeric(value),
    integer = as.integer(value),
    character = as.character(value)
  )
}


#' @keywords internal
#' @title Multiple inputs style
#' @description Multiple inputs separated by `,` as in KOLNAVN and MANHEADER
#' @param input The product from `find_column_input()`
is_col_separate <- function(input){
  args <- is_separate(input, ",")
  dt <- vector(mode = "list", length = length(args))

  for (i in seq_along(args)){
    x <- is_col_var(args[i])
    dt[[i]] <- x
  }

  cols <- unlist(dt)
  lhs <- cols[names(cols) == "old"]
  rhs <- cols[names(cols) == "new"]

  list(old = unname(lhs), new = unname(rhs))
}

is_col_var <- function(col){
  lhs <- is_separate(col, "=")[1]
  rhs <- is_separate(col, "=")[2]
  list(old = lhs, new = rhs)
}


#' @keywords internal
#' @title Convert column to numeric with warning
#' @description Covert to numeric for columns that are expected to be numeric
#'  and give warning and log when coercion
#' @param dt Dataset
#' @param cols Columns to be converted to numeric
#' @param ... Extra arguments
is_col_num_warn <- function(dt, cols, koblid = NULL){
  GEO <- NULL

  for (j in seq_len(length(cols))){
    col <- cols[j]
    if (methods::is(dt[[col]], "character")) {
      tryCatch({
        data.table::set(dt, j = col, value = as.numeric(dt[[col]]))
      },
      warning = function(x) {
        dumCol <- "dumCol"
        dt[, (dumCol) := get(col)]
        suppressWarnings(data.table::set(dt, j = dumCol, value = as.numeric(dt[[dumCol]])))
        notCodes <- dt[is.na(dumCol), GEO][[1]]
        dt[, (dumCol) := NULL]

        fileNA <- paste0(col, "xx")
        logCmd <- is_log_write(value = notCodes, x = fileNA, koblid = koblid)
        msg <- paste0("Check column ", col, "! NAs introduced by coercion!! Check codes with:")
        is_color_txt(logCmd, msg = msg, type = "warn2")
      })
    }
  }
  return(dt)
}

