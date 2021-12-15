#' @title Reshape from Long to Wide
#' @description Reshape the dataset from long format to wide format.
#' @param dt Dataset to be reshaped
#' @param respec Reshape specification with `id` and `value` variables. This
#'   is the output from `get_reshape_wide_spec()`
#' @family reshape functions
#' @export
do_reshape_wide <- function(dt = NULL, respec = NULL){
  resCol <- respec$rescol
  resVal <- respec$resval

  ## TODO select only specific folder as reshape id instead of all with ...

  data.table::dcast(dt, paste0("...", "~", resCol), value.var = resVal)
}

# 1 = LONG
# 2 = WIDE

#' @title Reshape Wide Specification
#' @description Get the formula and value variables for reshaping a long dataset
#'   to wide. For detail please read `data.table::dcast.data.table` to
#'   understand `formula` and `value` variables.
#' @inheritParams do_reshape
#' @inheritParams make_file
  #' @inheritParams get_split
  #' @inheritParams find_column_input
  #' @family reshape functions
  #' @export
  get_reshape_wide_spec <- function(dt = NULL, group = NULL, con = NULL, spec = NULL){

    is_debug()
    is_null_both(group, spec)
    is_not_null_both(group, spec)

    if (is.null(spec)) {
      spec <- find_spec("specification.sql", value = group, con = con)
    }

    ## TODO Delete or exclude column that should not be included as in RESHAPE_ID

    resCol <- spec[["RESHAPE_KOL"]]
    resVal <- spec[["RESHAPE_VAL"]]
    valCols <- as.character(unique(dt[[resCol]]))

    return(list(rescol = resCol,
                resval = resVal,
                valcols = valCols))
  }
