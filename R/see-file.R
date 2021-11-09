#' @title See Column Categories
#' @description See what categories available in the dataset.
#' @param dt Dataset of type `data.frame` or `data.table`
#' @param ... Columnames or column index to be displayed. If missing then all
#'   columns will be listed.
#' @examples
#' \dontrun{
#' DT <- make_file("BEFOLKNING")
#' # Use columnames
#' see_file(DT, KJONN, UTDANN, LANDSSB)
#'
#' dt <- read_file(15)
#' # Use column index
#' see_file(dt) #all columns
#' see_file(dt , c(2,5))
#' see_file(dt, c(1:3)) #columns 1 to 3
#' see_file(dt, c(2, 4, 7:9))
#' }
#' @export
see_file <- function(dt = NULL, ...){

  if (is(dt, "character")){
    obj <- class(dt)
    msg <- "Not able to read object of type"
    is_stop(msg, obj)
  }

  if (isFALSE(is(dt, "data.table"))) {
    data.table::setDT(dt)
  }

  cols <- eval(substitute(alist(...)))

  if (length(cols) > 0){
    cols <- is_variables(dt, cols)
  } else {
    cols <- names(dt)
  }

  out <- vector(mode = "list", length = length(cols))
  for (i in seq_along(cols)){
    col <- cols[i]
    grp <- dt[, .N, keyby = list(get(col))]
    data.table::setnames(grp, "get", col)
    out[[i]] <- grp
    names(out)[i] <- col
  }
  return(out)
}


#' @export
#' @rdname see_file
se_fil <- see_file

## Helper -----------------------
is_variables <- function(dt, cols){
  itm <- cols[[1]]

  if (is(itm, "character")){
    cols <- unlist(cols)
  } else if (is(itm, "call")) {
    idx <- eval(itm)
    cols <- names(dt)[idx]
  } else if (is(itm, "numeric")){
    cols <- names(dt)[itm]
  } else {
    cols <- sapply(as.list(cols), deparse)
  }

  return(cols)
}
