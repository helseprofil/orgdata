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
#' @param save Logical value. Default is `FALSE`. To save as `.csv` format file
#'   by activating `save_file()` function.
#' @inheritParams do_geo_recode
#' @param implicitnull Logical value. Default is `TRUE` to add implicit null to
#'   the dataset. Global options with `orgdata.implicit.null`.
#' @param row Select only specify row(s). Useful for debugging. Please read
#'   `Debugging` article for detail.
#' @inheritParams do_geo_recode
#' @param parallel Logical or numeric value. With logical value `TRUE` it will
#'   run with parallel using 50% ie. 0.5 of local cores. User can decide other
#'   percentage if needed. For example to use 75% of the cores then specify as
#'   `parallel = 0.75`. Nevertheless, maximum cores allowed is only 80%. Default
#'   value is `FALSE` ie. to use sequential processing
#' @param raw Logical value. Either to read original raw data directly even if
#'   the dataset is available in DuckDB without the need to unmark `KONTROLLERT`
#'   in the database
#' @aliases make_file lag_fil
#' @examples
#' \dontrun{
#' dt <- make_file("ENPERSON")
#' dt <- make_file("ENPERSON", raw = TRUE) #Skip DuckDB and read directly from original files
#' dt <- make_file("ENPERSON", koblid = 120:125, parallel = TRUE) #with parallel processing
#' }
#' @importFrom data.table `:=` `%chin%`
#' @importFrom crayon `%+%`
#' @family filegroups functions
#' @export
make_file <- function(group = NULL,
                      koblid = NULL,
                      aggregate = NULL,
                      save = FALSE,
                      year = NULL,
                      implicitnull = NULL,
                      row = NULL,
                      base = NULL,
                      parallel = NULL,
                      raw = FALSE
                      ) {

  LEVEL <- NULL

  is_null(group, "Filgruppe is missing")
  is_debug()

  if (is.null(aggregate)) aggregate <- getOption("orgdata.aggregate")
  if (is.null(year)) year <- getOption("orgdata.year")
  if (is.null(implicitnull)) implicitnull <- getOption("orgdata.implicit.null")
  if (is.null(row)) row <- getOption("orgdata.debug.row")
  if (is.null(base)) base <- getOption("orgdata.recode.base")
  if (is.null(parallel)) parallel <- getOption("orgdata.parallel")

  ## Use argument `raw` as standard value to avoid
  ## resetting the global options
  options(orgdata.read.raw = raw)

  dbFile <- is_path_db(
    db = getOption("orgdata.db"),
    check = TRUE
  )

  is_color_txt(year, "Production year for")

  ## CONNECTION --------------------------------------------
  ## Access
  kh <- is_conn_db(dbFile)
  on.exit(kh$db_close(), add = TRUE)

  ## DuckDB
  duck <- is_conn_db(dbname = group,
                     dbtype = "DuckDB",
                     dbyear = year)
  on.exit(duck$db_close(), add = TRUE)

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


  ## COLUMNS TO KEEP ---------------------------------------
  dataCols <- is_data_cols(fgspec = fgSpec)

  ## PROCESS ON FILES LEVEL IN A FILGRUPPE -----------------
  ## Parallel uses 50% of the cores but not more than 80%
  useCore <- 0.5
  if (is.numeric(parallel)){
    useParallel <- TRUE
    useCore <- min(parallel, 0.8)
  } else {
    useParallel <- parallel
  }

  options(parallelly.availableCores.custom = function() {
    ncores <- max(parallel::detectCores(), 1L, na.rm = TRUE)
    ncores <- min(as.integer(useCore * ncores))
    max(1L, ncores)
  })

  future::plan("multisession")


  if (useParallel){
    p <- progressr::progressor(steps = rowFile)
    paraMsg1 <- paste0("Start parallel processing ... \U001F680")
    paraMsg <- paste0("Start parallel processing with ", parallelly::availableCores(), " cores \U001F680")
    is_verbose(msg = paraMsg)

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
                                          base = base,
                                          duck = duck)},
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
                                     base = base,
                                     duck = duck)
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

  ## RENAME, CONVERT AND REORDER COLUMNS ----------------------
  standardCols <- is_standard_cols()
  orderCols <- intersect(standardCols, names(outDT))
  data.table::setcolorder(outDT, orderCols)

  #convert to numeric
  numCols <- c(getOption("orgdata.num"), paste0("VAL", 1:getOption("orgdata.vals")))
  outDT <- is_col_num_warn(outDT, numCols)

  ## EXTRA ARGUMENTS FOR FILEGROUP ---------------------------
  ## -- DELETE OLD BYDEL --
  bySpec <- get_extra_args_group(spec = fgSpec)
  outDT <- do_extra_args_group(dt = outDT, args = bySpec )

  ## POST RECODE ---------------------------------------------
  outDT <- do_recode_post(dt = outDT, spec = fgSpec, con = kh$dbconn)

  ## RENAME STANDARD COLUMNS ---------------------------------
  grpCols <- get_colname(spec = fgSpec)
  outDT <- do_colname(dt = outDT, cols = grpCols)

  ## Shut down parallel workers
  future::plan("sequential")

  if (save) {
    save_file(dt = outDT, name = group, fgSpec = fgSpec)
  }

  prodMsg <- paste0("Done! `", group ,"` for")

  is_colour_txt(x = year,
                msg = prodMsg,
                type = "note",
                emoji = TRUE,
                symbol = "thumb")

  withr::with_options(list(orgdata.emoji = "paper"),
                      is_verbose(x = is_orgdata_path(),
                                 msg = "Log files can be found in",
                                 emoji = TRUE))

  return(outDT[])
}

#' @export
#' @rdname make_file
lag_fil <- make_file


## HELPER -----------------------------------------
## Helper functions are in file utils-read-file.R
