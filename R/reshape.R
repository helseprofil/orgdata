## TODO Create column dummy for TAB and VAL if RESHAPE_KOL exist

do_reshape_rename_col <- function(dt = NULL, spec = NULL){
  is_debug()
  input <- find_column_input(spec = spec, "RESHAPE_KOL")

  if (!is.na(input)){
    input <- is_col_separate(input = input)
    input <- is_reshape_col_id(input = input)
    input <- is_reshape_col_val(input = input)
    ## TODO Give error if TAB1, VAL1 etc allready exist and specified in
    ## innlesing under TAB1 or VAL1 etc
    data.table::setnames(dt, old = input[["old"]], new = input[["new"]])
  }

  invisible(dt)
}

do_reshape <- function(dt = NULL, idvar = NULL){

  is_debug()
  dt <- data.table::melt(dt, id.vars = idvar$id, measure.vars = idvar$var)
  invisible(dt)
}

get_reshape_id_val <- function(dt = NULL, group = NULL, con = NULL, spec = NULL){

  is_debug()
  is_null_both(group, spec)
  is_not_null_both(group, spec)

  if (is.null(spec)) {
    spec <- find_spec("specification.sql", value = group, con = con)
  }

  dtNames <- names(dt)
  reshapeID <- find_reshape_id(spec = spec)
  resh <- find_reshape_type(spec)

  if (resh == "error"){
    is_stop("RESHAPE_VAL is not specified correctly or it has leading whitespace")
  }

  reshVars <- switch(resh,
                     all = is_reshape_var_all(dtnames = dtNames, reshapeid = reshapeID),
                     list = is_reshape_var_list(spec),
                     not = is_reshape_var_other(dtnames = dtNames, reshapeid = reshapeID, spec))

  list(id = reshapeID, var = reshVars)
}


find_reshape_id <- function(group = NULL, con = NULL, spec = NULL){

  is_debug()
  is_null_both(group, spec)
  is_not_null_both(group, spec)

  if (is.null(spec)) {
    spec <- find_spec("specification.sql", value = group, con = con)
  }

  find_column_multi(spec, "RESHAPE_ID", sep = ",")
}


find_reshape_type <- function(spec = NULL){
  input <- spec$RESHAPE_VAL

  if (!is.na(input)){
    resh <- is_reshape_input(input)
  } else {
    resh <- "all"
  }

  return(resh)
}


## HELPER ------------------------------
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

is_reshape_col_val <- function(input){
  vals <- c("VAL1", "VAL2", "VAL3")
  var <- intersect(input$old, vals)
  if (length(var) == 1){
    idx <- which(input$old == vals[1])
    input$old[idx] <- "value"
    input$new[idx] <- vals[1]
  } else if (length(var) > 1){
    idx <- which(input$old == vals[1])
    input$old[idx] <- "value1"
    input$new[idx] <- vals[1]
    idx <- which(input$old == vals[2])
    input$old[idx] <- "value2"
    input$new[idx] <- vals[2]
    idx <- which(input$old == vals[3])
    input$old[idx] <- "value3"
    input$new[idx] <- vals[3]
  }
  return(input)
}

is_reshape_input <- function(input){
  if (grepl("^list", input)){
    out <- "vars"
  } else if (grepl("^!", input)){
    out <- "not"
  } else {
    out <- "error"
  }
  return(out)
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
  vars <- gsub("^!\\((.*)\\)", "\\1", input)
  vars <- is_separate(vars, sep = ",")
  vars <- c(vars, reshapeid)
  setdiff(dtnames, vars)
}
