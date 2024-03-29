% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/file-process.R
\name{is_process_file}
\alias{is_process_file}
\title{Process Raw Data File}
\usage{
is_process_file(
  file,
  filespec,
  fgspec,
  con,
  verbose = NULL,
  row = NULL,
  control = FALSE,
  duck = NULL
)
}
\arguments{
\item{file}{File rawdata}

\item{filespec}{Specification for a file from \code{tbl_Innlesing}}

\item{fgspec}{Specification for a file group}

\item{con}{Connection to the database}

\item{verbose}{Make processes explicit. Default is FALSE}

\item{row}{Select specific row only}

\item{control}{Logical value. If the file has been checked for possible
errors}

\item{duck}{R6 object for DuckDB}
}
\value{
A dataset with \code{data.table} format
}
\description{
Implement the specifications to the raw data such as cleaning
and recoding the data.
}
\keyword{internal}
