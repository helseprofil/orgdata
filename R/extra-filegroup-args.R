
#' @title Execute Extra Arguments for Filegroup
#' @description This is based on the input in `EXTRA` column from Access
#'   registration database on filegroup. The arguments that are valid here can
#'   be expanded whenever needed. See details section for valid arguments to be
#'   used. All argument names are written in `CamelCase` style. Use symbol `|`
#'   to separate multiple arguments.
#'
#' @details Currently, these arguments can be used:
#'   - `DeleteOldBydel` : Delete bydel codes before 2003, except for Oslo
#' @param dt Dataset
#' @param args Extra arguments as specified in details section below.
#' @family extra arguments
#' @export
do_extra_args_group <- function(dt = NULL, args = NULL){
  dt <- is_delete_bydel_before_2003(dt, extra = args)
  dt <- is_age_category(dt, extra = args)

  return(dt)
}


#' @title Get Extra Arguments for Filegroup
#' @description This is based on the input in `EXTRA` column from Access
#'   registration database on filegroup. The arguments that are valid here can
#'   be expanded whenever needed. All argument names are written in `CamelCase`
#'   style. Use comma to seperate multiple arguments.
#' @inheritParams make_file
#' @inheritParams find_spec
#' @inheritParams find_column_input
#' @return A list with the names and value of arguments
#' @family extra arguments
#' @export

get_extra_args_group <- function(group = NULL, con = NULL, spec = NULL){

  is_debug()
  is_null_both(group, spec)
  is_not_null_both(group, spec)

  if (is.null(spec)) {
    spec <- find_spec("filegroups.sql", value = group, con = con)
  }

  input <- find_column_input(spec = spec, col = "EXTRA")

  if (!is.na(input)) {
    input <- find_column_multi(spec = spec, col = "EXTRA", sep = "|")
  }
  return(input)

}

## Arguments ----------------
# Delete older bydel ie. before 2003 except for Oslo
is_delete_bydel_before_2003 <- function(dt = NULL, extra = NULL){
  is_debug(deep = TRUE)
  delBydel <- any(extra == "DeleteOldBydel")

  if (isTRUE(delBydel)){
    LEVEL <- AAR <- GEO <- NULL
    dt <- data.table::copy(dt)
    idx <- dt[, .I[LEVEL == "bydel" & AAR < 2003 & !(GEO %like% "^3")]]
    dt <- is_delete_index(dt, idx)
  }
  return(dt)
}

is_age_category <- function(dt = NULL, extra = NULL){
  is_debug(deep = TRUE)

  ageCat <- extra[grepl("AgeCat", extra)]
  if (length(ageCat > 0)){

    gp <- input_age_class(ageCat)
    dt <- age_category(dt = dt, interval = gp)
  }

  return(dt)
}

input_age_class <- function(input){
  input <- sub("^AgeCat\\((.*)\\)", "\\1", input)

  inx <- is_separate(input, sep = ",")

  if (length(inx) > 1){
    # category with specified group
    class(inx) <- append(class(inx), "cat")

  } else {
    # category with interval eg. every 5 years
    inx <- tryCatch(as.numeric(inx),
                    warning = function(w){
                      is_stop("Interval for AgeCat in EXTRA is not numeric:", inx)
                    })
    class(inx) <- append(class(inx), "val")
  }

  return(inx)
}
