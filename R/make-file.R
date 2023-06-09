#' Implement the Specifications for Raw Files
#' @description Make a `csv` file with the specifications in the Access register
#'   database and implement them to the raw data of the selected group of files
#'   ie. (\emph{filgruppe}). All files under the selected group will be affected
#'   unless the \code{KOBLID} with argument \code{koblid} is specified or
#'   \code{select} argument is used. Specifying \code{koblid} or \code{select}
#'   is useful especially for testing purposes.
#' @description This function is the most used function in KHelse for processing
#'   raw data. The function [lag_fil()] is an alias to [make_file()].
#' @param group The name of filegroup as specified in \emph{filgruppe}
#' @param koblid \code{KOBLID} from table \emph{tbl_Koble}
#' @param aggregate Logical value. Default is `TRUE`. Aggregate data according
#'   to the specification in registration database. Global options with
#'   `orgdata.aggregate`.
#' @param save Logical value. Default is `FALSE`. To save as `.csv` format file
#'   by activating `save_file()` function.
#' @inheritParams do_geo_recode
#' @param implicitnull Logical value. Default is `TRUE` to add implicit null to
#'   the dataset. Global options with `orgdata.implicit.null`.
#' @param row Select specific row(s) numbers only. Useful for debugging. Please
#'   read `Debugging` article for detail.
#' @inheritParams do_geo_recode
#' @param parallel Logical or numeric value. With logical value `TRUE` it will
#'   run with parallel using 50% ie. 0.5 of local cores. User can decide other
#'   percentage if needed. For example to use 75% of the cores then specify as
#'   `parallel = 0.75`. Nevertheless, maximum cores allowed is only 80%. Default
#'   value is `FALSE` ie. to use sequential processing
#' @param raw Logical value. Default is `FALSE` as in config. If `TRUE` then
#'   read original raw data directly from source file even if the dataset is
#'   already available in DuckDB without the need to unmark `KONTROLLERT` in the
#'   Access database
#' @param select Select number of valid files to process as an alternative to
#'   using `KOBLID`. To select the first 5 files then write `select=1:5`. Use
#'   `select="last"` to select the last file.
#' @aliases make_file lag_fil
#' @examples
#' \dontrun{
#' dt <- make_file("ENPERSON")
#' dt <- make_file("ENPERSON", raw = TRUE) #Skip DuckDB and read directly from original files
#' dt <- make_file("ENPERSON", koblid = 120:125) #Select specific files only
#' dt <- make_file("ENPERSON", select = "last") #Select most recent file
#' }
#' @importFrom data.table `:=` `%chin%`
#' @importFrom lifecycle deprecated
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
                      parallel = deprecated(),
                      raw = NULL,
                      select = NULL
                      ) {

  LEVEL <- NULL

  is_null(group, "Filgruppe is missing")
  is_debug()

  if (is.null(aggregate)) aggregate <- getOption("orgdata.aggregate")
  if (is.null(year)) year <- getOption("orgdata.year")
  if (is.null(implicitnull)) implicitnull <- getOption("orgdata.implicit.null")
  if (is.null(row)) row <- getOption("orgdata.debug.row")
  if (is.null(base)) base <- getOption("orgdata.recode.base")
  if (is.null(raw)) raw <- getOption("orgdata.read.raw")

  if (lifecycle::is_present(parallel)){
    lifecycle::deprecate_stop(
      when = "0.6.8",
      what = "make_file(parallel)",
      details = "Parallel prosessing is now deprecated. Use `KONTROLLERT` solution instead for speed"
    )
  }

  ## Avoid resetting the global options
  withr::local_options(list(orgdata.read.raw = raw))

  dbFile <- is_path_db(
    db = getOption("orgdata.db"),
    check = TRUE
  )

  is_color_txt(year, "Start processing data with GEO mapping for")

  ## CONNECTION --------------------------------------------
  ## Access
  kh <- is_conn_db(dbFile)
  on.exit(kh$db_close(), add = TRUE)

  ## DuckDB
  # DuckDB is on active development and newer version isn't able to read
  # older duck. Therefore need to delete if the saved DuckDB is older version
  duck <- tryCatch({is_conn_db(dbname = group,
                               dbtype = "DuckDB",
                               dbyear = year)},
                   error = function(err){
                     ddk <- KHelse$new(dbname = group,
                                       dbtype = "DuckDB",
                                       dbyear = year,
                                       dbpath = TRUE,
                                       conn = FALSE)
                     fs::file_delete(ddk$dbpath)
                     rm(ddk)
                   })

  if (is.null(duck)){
    duck <- is_conn_db(dbname = group,
                       dbtype = "DuckDB",
                       dbyear = year)
  }

  on.exit(duck$db_close(), add = TRUE)

  ## SPECIFICATIONS ----------------------------------------
  today <- format(Sys.time(), "#%Y-%m-%d#")

  spec <- find_spec(
    file = "specification.sql",
    char = group,
    char2 = today,
    char3 = today,
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

  if (!is.null(select)){
    if (select == "last"){
      select <- max(nrow(spec))
    }
    spec <- spec[select,]
    rowFile <- nrow(spec)
    is_color_txt(length(select), "Number of file(s) to process:")
  }

  ## COLUMNS TO KEEP ---------------------------------------
  dataCols <- is_data_cols(fgspec = fgSpec)

  ## PROCESS ON FILES LEVEL IN A FILGRUPPE -----------------
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

  ## PROCESS ON FILGRUPPE LEVEL ----------------------------------
  outDT <- data.table::rbindlist(DT, fill = TRUE)
  rm(DT)
  invisible(gc(reset = TRUE))

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
  bySpec <- get_extra_args_group(spec = fgSpec)
  outDT <- do_extra_args_group(dt = outDT, args = bySpec )

  ## POST RECODE ---------------------------------------------
  outDT <- do_recode_post(dt = outDT, spec = fgSpec, con = kh$dbconn)

  ## RENAME STANDARD COLUMNS ---------------------------------
  grpCols <- get_colname(spec = fgSpec)
  outDT <- do_colname(dt = outDT, cols = grpCols)

  if (save) {
    save_file(dt = outDT, name = group, fgSpec = fgSpec)
  }

  is_color_txt(x = group,
               msg = "Completed filegroup:",
               type = "note",
               emoji = TRUE,
               symbol = "thumb")

  prodPath <- gsub("\\\\", "/", fgSpec$UTMAPPE)
  is_color_txt(x = prodPath,
               msg = "Output folder:",
               type = "note",
               emoji = TRUE,
               symbol = "folder")

  withr::with_options(list(orgdata.emoji = "paper"),
                      is_verbose(x = is_orgdata_path(),
                                 msg = "Log files can be found in",
                                 emoji = TRUE))

  if (orgEnv$status){
    cat("\n", praise::praise("${EXCLAMATION}! ${adjective}"), is_word("!!!"), is_party(), "\n\n")
  }

  return(outDT)
}

#' @export
#' @rdname make_file
lag_fil <- make_file

#' @export
#' @rdname make_file
mf <- make_file

## HELPER -----------------------------------------
## Helper functions are in file utils-read-file.R
