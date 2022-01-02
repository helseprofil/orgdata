#' Implement the Specifications
#' @description Make a `csv` file with the specifications in the register
#'   database and implement them to the raw data of the selected group of files
#'   (\emph{filgruppe}). All files under the selected group will be affected
#'   unless the \code{KOBLID} with argument \code{koblid} is specified.
#'   Specifying \code{koblid} is useful especially for testing purposes.
#' @description The function [lag_fil()] is an alias to [make_file()].
#' @examples \dontrun{ make_file("BEFOLKNING") make_file("BEFOLKNING", koblid =
#'   19) make_file("BEFOLKNING", koblid = c(15, 50)) }
#' @param group The group of files (\emph{filgruppe})
#' @param koblid \code{KOBLID} from table \emph{tbl_Koble}
#' @param aggregate Logical argument. Default is `TRUE`. Aggregate data
#'   according to the specification in registration database. Global options
#'   with `orgdata.aggregate`.
#' @param save Save as `.csv` by activating `save_file()`. Default is `FALSE`
#' @inheritParams do_aggregate
#' @param implicitnull Logical argument. Default is `TRUE` to add implicit null
#'   to the dataset. Global options with `orgdata.implicit.null`.
#' @param row Select only specify row(s). Useful for debugging
#' @inheritParams do_geo_recode
#' @param parallel Logical argument. Either to run with parallel or not. Default
#'   is `FALSE`
#' @aliases make_file lag_fil
#' @importFrom data.table `:=` `%chin%`
#' @importFrom crayon `%+%`
#' @family filegroups functions
#' @export
make_file <- function(group = NULL,
                      koblid = NULL,
                      aggregate = getOption("orgdata.aggregate"),
                      save = FALSE,
                      year = getOption("orgdata.year"),
                      implicitnull = getOption("orgdata.implicit.null"),
                      row = getOption("orgdata.debug.row"),
                      base = getOption("orgdata.recode.base"),
                      parallel = FALSE
                      ) {

  LEVEL <- NULL

  is_null(group, "Filgruppe is missing")
  is_debug()

  dbFile <- is_path_db(
    db = getOption("orgdata.db"),
    check = TRUE
  )

  ## CONNECTION --------------------------------------------
  kh <- is_conn_db(dbFile)
  on.exit(kh$db_close(), add = TRUE)

  ## SPECIFICATIONS ----------------------------------------
  spec <- find_spec(
    file = "specification.sql",
    value = group,
    con = kh$dbconn
  )
  ## data.table::setDT(spec)
  ## TODO Can't use DT yet. Some functions are still
  ## based on DF eg. find_column_input

  fgSpec <- find_spec(
    file = "filegroups.sql",
    value = group,
    con = kh$dbconn
  )
  ## data.table::setDT(fgSpec)

  ## SELECT FILES ------------------------------------------
  spec <- is_org_files(spec = spec, id = koblid)
  rowFile <- nrow(spec)
  grpMsg <- paste0("File(s) to be processed in ", group, ":")
  is_colour_txt(x = rowFile, grpMsg, type = "note")

  is_verbose(x = is_orgdata_path(), msg = "Log files can be found in")

  ## COLUMNS TO KEEP ---------------------------------------
  dataCols <- is_data_cols(fgspec = fgSpec)

  ## PROCESS ON FILES LEVEL IN A FILGRUPPE -----------------------
  if(parallel){
    is_verbose(msg = "Start parallel processing ...")
    future::plan(future::multisession)
    p <- progressr::progressor(steps = rowFile)
    ## p <- progressr::progressor(along = seq_len(rowFile))
    ## progressr::handlers(global = TRUE) #to enable progressor globally
  }

  if (parallel){
    DT <- future.apply::future_lapply(seq_len(rowFile),
                                      function(x) {
                                        p()
                                        do_make_file_each(i = x,
                                                          spec = spec,
                                                          fgspec = fgSpec,
                                                          aggregate = aggregate,
                                                          datacols = dataCols,
                                                          year = year,
                                                          row = row,
                                                          base = base)},
                                      future.seed = TRUE)
  } else {

    DT <- listenv::listenv()
    for (i in seq_len(rowFile)) {
      DT[[i]] <- do_make_file_each(i = i,
                                   spec = spec,
                                   fgspec = fgSpec,
                                   aggregate = aggregate,
                                   datacols = dataCols,
                                   year = year,
                                   row = row,
                                   base = base)
    }

    DT <- as.list(DT)
  }

  ## PROCESS ON FILGRUPPE LEVEL ----------------------------------
  outDT <- data.table::rbindlist(DT, fill = TRUE)
  rm(DT)

  if (getOption("orgdata.debug.geo")){
    return(outDT)
  }

  geoLevel <- get_aggregate(spec = fgSpec)

  is_verbose(msg = is_line_short(), type = "other", ctrl = FALSE)

  if (implicitnull){
    for(gg in geoLevel){
      dtsub <- outDT[LEVEL == gg]
      dnull <- do_implicit_null(dtsub, level = gg)
      if (nrow(dnull) > 0){
        impMsg <- sprintf("Number of row(s) with implicit null for %s:", gg)
        is_verbose(x = nrow(dnull), msg = impMsg)
        outDT <- data.table::rbindlist(list(outDT, dnull))
      }
    }
  }

  data.table::setkeyv(outDT, c("GEO", "AAR"))

  is_verbose(msg = is_line_short(), type = "other", ctrl = FALSE)
  outDT <- do_recode_aggregate(dt = outDT,
                               spec = fgSpec,
                               con = kh$dbconn)

  standardCols <- is_standard_cols()
  orderCols <- intersect(standardCols, names(outDT))
  data.table::setcolorder(outDT, orderCols)

  grpCols <- get_colname(spec = fgSpec)
  outDT <- do_colname(dt = outDT, cols = grpCols)

  ## DELETE OLD BYDEL ---------------------------
  bySpec <- get_extra_args_group(spec = fgSpec)
  outDT <- do_extra_args_group(dt = outDT, args = bySpec )


  if (save) {
    save_file(dt = outDT, name = group, fgSpec = fgSpec)
  }

  return(outDT[])
}

  #' @export
  #' @rdname make_file
lag_fil <- make_file


## Helper -----------------------------------------
## Helper functions are in file utils-read-org.R
