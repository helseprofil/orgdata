#' @title Recode Geographical Codes
#' @description Recode geographical codes to the current year.
#'  Codes is based [norgeo::track_change()] function.
#' @inheritParams do_split
#' @param code Code dataset of old and new codes in a `data.table` format.
#'  The dataset is the output after running `get_geo_recode()` function.
#' @examples
#' \dontrun{
#' code <- get_geo_recode(con = geo$dbconn, type = "grunnkrets")
#' dt <- make_file("BEFOLKNING", aggregate = FALSE)
#' DT <- do_geo_recode(dt, code)
#' }
#' @import data.table
#' @export
do_geo_recode <- function(dt = NULL,
                          code = NULL){
  GEO <- i.to <- NULL

  ## Ensure variables to be used to aggregate in type int
  intVar <- c("GEO", "VAL1")
  for (col in intVar){
    data.table::set(dt, j = col, value = as.integer(dt[[col]]))
  }

  dt <- is_geo_na(dt)
  dt[code, on = "GEO", GEO := i.to]

}

#' @title Get Previous and Current Geo Codes
#' @description Get the geographical codes registered in `geo-database` which consist
#'  of old and new codes that are applicable to the respective year.
#' @inheritParams find_spec
#' @param type The geographical granularity for recoding
#' @param year Which year the geograhical codes to be recoded to. If it
#'  is empty then current year will be used.
#' @return A dataset with columns `GEO` and `to` representing the GEO
#'  codes that will be recoded to a new code ie. `to`.
#' @import data.table
#' @export
get_geo_recode <- function(con = NULL,
                           type = c(
                             "grunnkrets",
                             "fylke",
                             "kommune",
                             "bydel"),
                           year = NULL){

  changeOccurred <- NULL

  is_debug()
  is_null(con)
  is_null(year)
  type <- match.arg(type)

  if (is.null(year)) {
    year <- as.integer(format(Sys.Date(), "%Y"))
  }

  table <- paste0(type, year)
  geoDT <- find_spec("geo-recode.sql", value = table, con = con)
  data.table::setDT(geoDT)

  for (j in seq_len(ncol(geoDT))){
    if (class(geoDT[[j]]) == 'character')
      data.table::set(geoDT, j = j, value = as.integer(geoDT[[j]]))
  }
  geoDT[, changeOccurred := NULL]
  data.table::setnames(geoDT, c("oldCode", "currentCode"), c("GEO", "to"))
}

## Helper -----------------
is_geo_na <- function(dt){
  GEO <- AAR <- NULL

  nrNA <- dt[is.na(GEO), .N]
  if (nrNA > 0){
    dt[is.na(GEO), GEO := 99999999]
  }

  if (nrNA > 0){
    is_colour_txt(x = nrNA, msg = "Number of missing GEO with empty value or NA:", type = "note")
    is_colour_txt(x = 99999999, msg = "Missing GEO are now recoded to", type = "warn")
    }
  invisible(dt)
}
