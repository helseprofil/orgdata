% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/norgeo-recode.R
\name{do_recode_without_aggregate}
\alias{do_recode_without_aggregate}
\title{Recode Geo Code Without Aggregate}
\usage{
do_recode_without_aggregate(
  dt = NULL,
  source = c("grunnkrets", "fylke", "kommune", "bydel"),
  year = NULL,
  base = NULL,
  ...
)
}
\arguments{
\item{dt}{Input data preferrably in a \code{data.table} format but is not mandatory}

\item{source}{What geographical granularity codes that is available in the
source data. This will be used for merging with the output from
\code{geo_map()}}

\item{year}{Which year the geograhical codes to be recoded to. If it is empty
then global option for \code{orgdata.year} will be used.}

\item{base}{Logical value. If \code{TRUE} then use year in the original data as the base
year to recode the geographical codes. Default is \code{FALSE} and use all
available codes in geo codebook}

\item{...}{Any additional arguments}
}
\description{
Recode geo code without aggregating the data.
The input in argument \code{source} must be a lower
granularity level than the \code{level} input.
}
\examples{
\dontrun{
# Source data with enumeration area codes ie. grunnkrets
dt <- make_file("BEFOLKNING", aggregate = FALSE)
}
}
\seealso{
Other geo recode functions: 
\code{\link{do_geo_recode}()},
\code{\link{get_geo_recode}()}
}
\concept{geo recode functions}
