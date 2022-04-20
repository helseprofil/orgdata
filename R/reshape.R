#' @title Rename Reshaped Column
#' @description Reshaping variables in the dataset from wide to long will
#'   produce ID and value columns. This function will rename these columns when
#'   specified under `RESHAPE_KOL` in the Access registration database.
#' @param dt Dataset that has been reshaped
#' @param spec Specification in column `RESHAPE_KOL` as in `tbl_Innlesing`
#' @family reshape functions
#' @export
do_reshape_rename_col <- function(dt = NULL, spec = NULL){
  is_debug()

  input <- find_column_input(spec = spec, "RESHAPE_KOL")

  if (!is.na(input)){
    input <- is_col_separate(input = input)
    input <- is_reshape_col_id(input = input)
    input <- is_reshape_col_val(input = input)
    ## TODO Give error if TAB1, VAL1 etc allready exist and specified in
    ## innlesing under TAB1 or VAL1 etc
    tryCatch(
      data.table::setnames(dt, old = input[["old"]], new = input[["new"]]),
      error = function(err){
        allColNames <- setdiff(names(dt), c(getOption("orgdata.columns"), "LANDBAK", "INNVKAT"))
        colNames <- is_long_vector(allColNames)
        is_color_txt(spec$RESHAPE_KOL, msg = "Your defined RESHAPE_KOL in Access is:", type = "error")
        is_color_txt(is_long_vector(input$old), "Selected column(s) to rename from:")
        is_color_txt(is_long_vector(input$new), "Selected column(s) to rename to:")
        is_color_txt(x = colNames, msg = "Available columnames to rename in the dataset:")
        is_stop("Please redefine RESHAPE_ID, RESHAPE_KOL or RESHAPE_VAL!", "")
      }
    )
  }

  return(dt)
}

#' @title Reshape from Wide to Long
#' @description Reshape the dataset from wide format to long format.
#' @param dt Dataset to be reshaped
#' @param respec Reshape specification with `id`, `measure` and `reshape type`
#'   variables. This is the output from `get_reshape_id_val()`
#' @family reshape functions
#' @export
do_reshape <- function(dt = NULL, respec = NULL){

  is_debug()

  idCols <- respec$id
  varCols <- respec$var

  if (length(idCols) == 0){
    return(dt)
  }

  ## varID <- any(idCols %in% paste0("VAL", 1:getOption("orgdata.vals")))
  ## if (varID){
  ##   is_color_txt("", msg = "VAL column found in RESHAPE_ID!", type = "warn")
  ##   is_stop("Your defined RESHAPE_KOL is", idCols)
  ## }

  ## This is when more than 1 column for VAL
  if (respec$type == "list"){
    lenCols <- length(varCols)
    listCols <- vector("list", length = lenCols)
    for (i in seq_len(lenCols)){
      col <- is_separate(varCols[i], sep = ",")
      listCols[[i]] <- col
    }
    dt <- data.table::melt(dt, id.vars = idCols, measure.vars = listCols)
  } else {
    dt <- data.table::melt(dt, id.vars = idCols, measure.vars = varCols)
  }

  return(dt)
}

#' @title Reshape Id and Measure
#' @description Get the id and measure variables for reshaping the dataset. For
#'   detail please read `data.table::melt.data.table` to understand `id` and
#'   `mearsure` variables.
#' @inheritParams do_reshape
#' @inheritParams make_file
#' @inheritParams get_split
#' @inheritParams find_column_input
#' @family reshape functions
#' @export
get_reshape_id_val <- function(dt = NULL, group = NULL, con = NULL, spec = NULL){

  is_debug()
  is_null_both(group, spec)
  is_not_null_both(group, spec)

  if (is.null(spec)) {
    spec <- find_spec("specification.sql", value = group, con = con)
  }

  if (is.na(spec$RESHAPE_ID)){
    return(list(id = NA))
  }

  dtNames <- names(dt)
  reshapeID <- is_reshape_id(spec = spec)
  resh <- is_reshape_type(spec)

  if (resh == "error"){
    is_stop("RESHAPE_VAL is not specified correctly or it has leading whitespace")
  }

  reshVars <- switch(resh,
                     all = is_reshape_var_all(dtnames = dtNames, reshapeid = reshapeID),
                     list = is_reshape_var_list(spec),
                     not = is_reshape_var_other(dtnames = dtNames, reshapeid = reshapeID, spec))

  list(id = reshapeID, var = reshVars, type = resh)
}


## HELPER ------------------------------
is_reshape_id <- function(group = NULL, con = NULL, spec = NULL){

  is_null_both(group, spec)
  is_not_null_both(group, spec)

  if (is.null(spec)) {
    spec <- find_spec("specification.sql", value = group, con = con)
  }

  find_column_multi(spec, "RESHAPE_ID", sep = ",")
}


is_reshape_type <- function(spec = NULL){

  input <- spec$RESHAPE_VAL

  if (!is.na(input)){
    resh <- is_reshape_input(input)
  } else {
    resh <- "all"
  }

  return(resh)
}

## When reshape the data, columns containing the categories will be called
## `variable`. This will be renamed to TAB1
is_reshape_col_id <- function(input){
  tab1 <- "TAB1"
  var <- intersect(input$old, tab1)
  if (length(var)!=0){
    idx <- which(input$old == tab1)
    input$old[idx] <- "variable"
    input$new[idx] <- tab1
  }
  return(input)
}

## When reshape the data, column containing the values will be called `value` if
## it's only one column and `value1` etc. when several columns with values. This
## will be renamed to VAL1, VAL2 and VAL3 when relevant
is_reshape_col_val <- function(input){
  vals <- paste0("VAL", 1:getOption("orgdata.vals"))
  var <- intersect(input$old, vals)
  is_reshape_val(input, var, vals)
}

is_reshape_val <- function(input, var, vals){
  if (length(var) > 1){
    for (i in seq_along(vals)){
      idx <- which(input$old == vals[i])
      input$old[idx] <- paste0("value", i)
      input$new[idx] <- vals[i]
    }
  } else {
    idx <- which(input$old == vals[1])
    input$old[idx] <- "value"
    input$new[idx] <- vals[1]
  }
  return(input)
}


is_reshape_input <- function(input){
  if (grepl("^list", input)){
    out <- "list"
  } else if (grepl("^-", input)){
    out <- "not"
  } else {
    out <- "error"
  }
}

is_reshape_var_all <- function(dtnames, reshapeid){
  setdiff(dtnames, reshapeid)
}

is_reshape_var_list <- function(spec){
  input <- spec$RESHAPE_VAL
  vv <- gsub("list", "", input)
  v2 <- strsplit(vv, "),")
  v2 <- unlist(v2)
  v3 <- gsub("\\(", "", v2)
  v4 <- gsub("\\)", "", v3)
  trimws(v4)
}

is_reshape_var_other <- function(dtnames, reshapeid, spec){
  input <- spec$RESHAPE_VAL
  vars <- gsub("^-\\((.*)\\)", "\\1", input)
  vars <- is_separate(vars, sep = ",")
  vars <- c(vars, reshapeid)
  setdiff(dtnames, vars)
}
