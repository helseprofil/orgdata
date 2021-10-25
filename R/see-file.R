#' @title See Column Categories
#' @description See what categories available in the dataset.
#' @param dt Dataset of type `data.frame` or `data.table`
#' @param ... Columnames or column index to be displayed
#' @examples
#' \dontrun{
#' # Use columnames
#' see_file(DT, KJONN, UTDANN, LANDSSB)
#'
#' # Use column index
#' see_file(DT, c(2,5))
#' see_file(DT, c(1:3))
#' see_file(DT, c(2, 4, 7:8))
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
  }

  out <- vector(mode = "list", length = length(cols))
  for (i in seq_along(cols)){
    col <- cols[i]
    grp <- dt[, .N, by = list(get(col))]
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
  } else {
    cols <- sapply(as.list(cols), deparse)
  }

  return(cols)
}
