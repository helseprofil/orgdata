#' @title Create Implicit Null
#' @description Implicit Null happens when a variable in a dataset has unequal
#'   number of categories in different years due to one or several of the categories
#'   have zero case.
#' @description All variables except `AAR` and `GEO` must have equal number of
#'   categories. The data that is handled by this package is an aggregated
#'   rawdata. The categories for each variables might be different in different
#'   year. This does not elucidate non-existence of the category but rather a
#'   zero number belonging to that category. This function will standardize the
#'   categories across all year.
#' @param dt Dataset consisting all years
#' @family implicit-null functions
#' @export
do_implicit_null <- function(dt){
  is_debug()
  is_verbose(msg = "Checking for implicit null ...")

  stdVals <- paste0("VAL", 1:getOption("orgdata.vals"))
  ignoreCols <- c("GEO", "AAR", stdVals)
  dtCols <- names(dt)
  cols <- setdiff(dtCols, ignoreCols)
  years <- sort(unique(dt[["AAR"]]))

  ## Need to keep the class type before return
  dtStr <- dt[, lapply(.SD, class)]

  refs <- get_implicit_ref(dt, cols = cols)
  imp <- get_implicit_col(dt, years, cols, refs)
  dtImp <- get_implicit_per_year(imp = imp, refs = refs, years, colstr = dtStr )

  colNum <- names(dtStr)[dtStr == "numeric"]
  for (j in colNum){
    data.table::set(dtImp, j=j, value = as.numeric(dtImp[[j]]))
  }

  for (j in seq_along(names(refs))){
    col <- names(refs)[j]
    val <- refs[[col]][1]
    data.table::set(dtImp, which(is.na(dtImp[[col]])), j = col, value = val)
  }
  invisible(dtImp)
}


#' @title Get Reference Values
#' @description Get the valid values for columns that will be controlled for
#' any possible implicit null.
#' @inheritParams do_implicit_null
#' @param cols Columns to be controlled for implicit null
#' @family implicit-null functions
#' @export
get_implicit_ref <- function(dt, cols){
  vcol <- vector(mode = "list", length = length(cols))
  for (i in seq_along(cols)){
    var <- cols[i]
    vUnik <- sort(unique(dt[[var]]))
    vcol[[i]] <- vUnik
    names(vcol)[i] <- as.character(var)
  }
  invisible(vcol)
}

#' @title Get Implicit Null Categories
#' @description Get categories with implicit values for each selected columns
#' @inheritParams do_implicit_null
#' @param years All years in the dataset
#' @inheritParams get_implicit_ref
#' @param refs Reference values for the selected columns
#' @family implicit-null functions
#' @export
get_implicit_col <- function(dt, years, cols, refs){
  nn <- vector(mode = "list", length = length(cols))
  for (i in seq_along(cols)){
    col <- cols[i]
    ref <- refs[[i]]
    dtn <- find_implicit_col(dt, years, col, ref)
    nn[[i]] <- dtn
    names(nn)[i] <- col
  }
  invisible(nn)
}


#' @title Dataset for Implicit Null by Year
#' @description Create a dataset with implicit null for each available
#' year in the dataset
#' @param imp A list of implicit null data derived from [get_implicit_col] function
#' @inheritParams get_implicit_col
#' @param colstr Column structure or Class type
#' @family implicit-null functions
#' @export
get_implicit_per_year <- function(imp, refs, years, colstr){
  dty <- vector(mode = "list", length = length(years))

  for (i in seq_len(length(years))){
    yr <- as.character(years[i])
    dd <- find_implicit_null(imp = imp, year = yr, colstr = colstr)
    dt <- data.table::rbindlist(dd)
    dty[[i]] <- dt
  }
  DT <- data.table::rbindlist(dty)
  invisible(DT)

}

#' @title Find Implicit Null Categories
#' @description Find implicit null categories for selected columns on every selected year
#' @inheritParams get_implicit_col
#' @param col Selected column to be controlled for implicit null
#' @param ref Reference values for the selected column
#' @family implicit-null functions
#' @export
find_implicit_col <- function(dt, years, col, ref) {
  nn <- vector(mode = "list", length = length(years))
  for (i in seq_along(years)){
    yr <- years[i]
    vars <- sort(unique(dt[[col]][dt[["AAR"]] == yr]))
    dd <- setdiff(ref, vars)
    nn[[i]] <- dd
    names(nn)[i] <- as.character(yr)
  }
  invisible(nn)
}

#' @title Dataset for Implicit Null
#' @description Create a dataset with implicit null for every selected columns
#' @inheritParams get_implicit_per_year
#' @param year Selected year from the dataset
#' @family implicit-null functions
#' @export
find_implicit_null <- function(imp, year, colstr){
  nn <- vector(mode = "list", length = length(imp))
  yr <- as.character(year)

  for (i in seq_along(names(imp))){
    col <- names(imp)[i]
    data <- imp[[col]]
    vals <- data[[yr]]

    dtTemp <- data.table::setnames(data.table::data.table(matrix(
      nrow = length(vals), ncol = length(colstr))), names(colstr))

    ## Need to be character to add new rows else all columns are logical and accept only 0/1
    for (j in names(dtTemp)) {
      data.table::set(dtTemp, j=j, value = as.character(dtTemp[[j]]))
    }

    dtTemp[, `:=`(GEO = "99999999", AAR = yr, VAL1 = "0")]
    dtTemp[, (col) := vals]

    nn[[i]] <- dtTemp
    names(nn)[i] <- col
  }

  invisible(nn)
}
