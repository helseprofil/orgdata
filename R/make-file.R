#' Implement the Specifications
#' @description Make a `csv` file with the specifications in the register
#'   database and implement them to the raw data of the selected group of files
#'   (\emph{filgruppe}). All files under the selected group will be affected
#'   unless the \code{KOBLID} with argument \code{koblid} is specified.
#'   Specifying \code{koblid} is useful especially for testing purposes.
#' @description The function [lag_fil()] is an alias to [make_file()].
#' @param group The group of files (\emph{filgruppe})
#' @param koblid \code{KOBLID} from table \emph{tbl_Koble}
#' @param aggregate Logical value. Default is `TRUE`. Aggregate data according
#'   to the specification in registration database. Global options with
#'   `orgdata.aggregate`.
#' @param save Save as `.csv` by activating `save_file()`. Default is `FALSE`
#' @inheritParams do_aggregate
#' @param implicitnull Logical value. Default is `TRUE` to add implicit null to
#'   the dataset. Global options with `orgdata.implicit.null`.
#' @param row Select only specify row(s). Useful for debugging
#' @inheritParams do_geo_recode
#' @param parallel Logical or numeric value. With logical value `TRUE` it will
#'   run with parallel using 50% ie. 0.5 of local cores. User can decide other
#'   percentage if needed. For example to use 75% of the cores then specify as
#'   `parallel = 0.75`. Nevertheless, maximum cores allowed is only 80%.
#'   Default value is `FALSE` ie. to use sequential processing
#' @aliases make_file lag_fil
#' @examples
#' \dontrun{
#' dt <- make_file("ENPERSON")
#' dt <- make_file("ENPERSON", koblid = 120:125, parallel = TRUE) #with parallel processing
#' }
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
                      parallel = getOption("orgdata.parallel")
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
  withr::with_options(list(orgdata.emoji = "book"),
                      is_colour_txt(x = rowFile, grpMsg, type = "note", emoji = TRUE))

  withr::with_options(list(orgdata.emoji = "folder"),
                      is_verbose(x = is_orgdata_path(), msg = "Log files can be found in", emoji = TRUE))

  ## COLUMNS TO KEEP ---------------------------------------
  dataCols <- is_data_cols(fgspec = fgSpec)

  ## PROCESS ON FILES LEVEL IN A FILGRUPPE -----------------------

  ## Parallel uses 50% of the cores but not more than 80%
  useCore <- 0.5
  if (is.numeric(parallel)){
    useParallel <- TRUE
    useCore <- min(parallel, 0.8)
  } else {
    useParallel <- parallel
  }

  if(useParallel){

    options(parallelly.availableCores.custom = function() {
      ncores <- max(parallel::detectCores(), 1L, na.rm = TRUE)
      ncores <- min(as.integer(useCore * ncores))
      max(1L, ncores)
    })
    paraMsg1 <- paste0("Start parallel processing ... \U001F680")
    paraMsg <- paste0("Start parallel processing with ", parallelly::availableCores(), " cores \U001F680")
    is_verbose(msg = paraMsg)

    future::plan(future::multisession)
    p <- progressr::progressor(steps = rowFile)
  }

  if (useParallel){
    DT <- future.apply::future_lapply(seq_len(rowFile),
                                      function(x) {
                                        p()
                                        Sys.sleep(0.01)
                                        do_make_file_each(
                                          spec = spec[x,],
                                          fgspec = fgSpec,
                                          aggregate = aggregate,
                                          datacols = dataCols,
                                          year = year,
                                          row = row,
                                          base = base)},
                                      future.seed = TRUE)
  } else {
    DT <- lapply(seq_len(rowFile),
                 function(x) {
                   do_make_file_each(spec = spec[x,],
                                     fgspec = fgSpec,
                                     aggregate = aggregate,
                                     datacols = dataCols,
                                     year = year,
                                     row = row,
                                     base = base)
                 })
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
