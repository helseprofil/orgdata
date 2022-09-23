#' @title Execute Extra Arguments for Filegroup
#' @description This is based on the input in `EXTRA` column from Access
#'   registration database on filegroup. The arguments that are valid here can
#'   be expanded whenever needed. See details section for valid arguments to be
#'   used. All argument names are written in `CamelCase` style.
#' @description NB! Use symbol `|` to separate multiple arguments eg. `DeleteOldBydel | AgeCat(5)`
#'
#' @details Currently, these arguments can be used:
#'   - `DeleteOldBydel` : Delete bydel codes before 2003, except for Oslo
#'   - `AgeCat()` : Categorise age to different groups with defined interval.
#'      Skal hovedsakelig legges inn på teller-filer i orgdata, på filer som skal aldersstandardiseres.
#'      KHfunctions vil selv tilpasse aldersgruppene i en eventuell nevnerfil. Example can be found in `find_age_category()`.
#' @param dt Dataset
#' @param args Extra arguments as specified in details section below.
#' @family extra arguments
#' @export
do_extra_args_group <- function(dt = NULL, args = NULL){

  is_debug()

  for (i in args){
    dt <- is_delete_bydel_before_2003(dt, extra = i)
    dt <- is_age_category(dt, extra = i)
  }

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

    gp <- is_input_age_class(ageCat)
    dt <- find_age_category(dt = dt, interval = gp)
  }

  return(dt)
}


## Helper ---------------
is_input_age_class <- function(input){
  input <- sub("^AgeCat\\((.*)\\)", "\\1", input)
  # mix - For mix class ie. AgeCat(0, 5, [10], 55, 60)
  mix <- any(grepl("\\[.*\\]", input))

  if (mix){
    input <- is_age_mix(input)
  } else {
    input <- is_age_not_mix(input)
  }
  return(input)
}

is_check_age_input <- function(inx){
  inx <- trimws(inx)
  inx <- tryCatch(as.numeric(inx),
                  warning = function(w){
                    is_stop("Interval for AgeCat in EXTRA is not numeric:", inx)
                  })

  inx[!is.na(inx)]
}

is_age_not_mix <- function(input){

  input <- is_separate(input, sep = ",")

  if (length(input) > 1){
    # category with specified group
    input <- is_check_age_input(input)
    class(input) <- append(class(input), "cat")

  } else {
    # category with interval eg. every 5 years
    input <- is_check_age_input(input)
    class(input) <- append(class(input), "val")
  }

  return(input)
}

is_age_mix <- function(input){

  input <- is_separate(input, "\\[", fixed = FALSE)
  lhs <- is_separate(input[1], ",")
  lhs <- is_check_age_input(lhs)

  val <- is_separate(input[2], "\\]", keep = 1, fixed = FALSE)
  val <- is_check_age_input(val)

  rhs <- is_separate(input[2], "\\]", keep = 2, fixed = FALSE)
  rhs <- is_separate(rhs, ",")
  rhs <- is_check_age_input(rhs)

  intVal <- c(seq(from = lhs[length(lhs)], to = rhs[1], by = val))
  intVal <- intVal[-c(1, length(intVal))]

  input <- c(lhs, intVal, rhs)
  class(input) <- append(class(input), "mix")
  return(input)
}
