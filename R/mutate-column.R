#' @title Create Column
#' @description Create new column with the value when specified between a
#'   symbols less then `<` and more than `>` in the standard columns. For
#'   example when specified under `KJONN` as `<2>`, a new column `KJONN` is
#'   created with value 2.
#' @description This function is only applicable for columns `"GEO", "AAR",
#'   "KJONN", "ALDER", "UTDANN", "LANDSSB"`.
#' @inheritParams do_split
#' @family mutate-column functions
#' @export
do_mutate <- function(dt = NULL, spec = NULL){
  dt <- copy(dt)

  cols <- c("GEO", "AAR", "KJONN", "ALDER", "UTDANN", "LANDSSB")
  for (i in cols){
    col <- spec[[i]]
    val <- get_mutate_value(col)
    is_mutate(dt, spec, col, val)
  }

  invisible(dt)
}

#' @title Value for New Column
#' @description Get the value for the new column if the column
#'   fullfilled the condition as a new column.
#' @param col The selected column
#' @family mutate-column functions
#' @export
get_mutate_value <- function(col){
  ja <- grepl("^<.*>$", col)
  if (ja){
    ut <- gsub("<(.+)>$", "\\1", col)
  } else {
    ut <- FALSE
  }
  return(ut)
}

## HELPER ------------------------------
is_mutate <- function(dt, spec, col, val){
  if (!isFALSE(val)){
    var <- names(spec)[spec %in% col]
    dt[, (var) := val]
  }
  invisible(dt)
}
