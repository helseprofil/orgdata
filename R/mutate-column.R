#' @title Create Column
#' @description Create new column with the value when specified between a
#'   symbols less then `<` and more than `>` in the standard columns. For
#'   example when specified under `KJONN` as `<2>`, a new column `KJONN` is
#'   created with value 2.
#' @description This function is applicable for columns `"GEO", "AAR",
#'   "KJONN", "ALDER", "UTDANN", "LANDSSB"`, all `"TABS"` and all `"VALS"`.
#' @inheritParams do_split
#' @inheritParams find_column_input
#' @family mutate-column functions
#' @export
do_mutate <- function(dt = NULL, spec = NULL){
  is_debug()

  dt <- copy(dt)

  colVals <- paste0("VAL", 1:getOption("orgdata.vals"))
  colTabs <- paste0("TAB", 1:getOption("orgdata.tabs"))
  cols <- c("GEO", "AAR", "KJONN", "ALDER", "UTDANN", "LANDSSB", colTabs, colVals)
  for (i in cols){
    col <- spec[[i]]
    val <- get_mutate_value(col)
    dt <- is_mutate(dt, spec, col, val)
  }
  return(dt)
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
    ut <- gsub("^<(.+)>$", "\\1", col)
  } else {
    ut <- FALSE
  }
}

## HELPER ------------------------------
is_mutate <- function(dt, spec, col, val){
  if (!isFALSE(val)){
    var <- names(spec)[spec %in% col]
    dt[, (var) := val]
  }
  return(dt)
}
