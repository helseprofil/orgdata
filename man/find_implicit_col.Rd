% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/implicit-null.R
\name{find_implicit_col}
\alias{find_implicit_col}
\title{Find Implicit Null Categories}
\usage{
find_implicit_col(dt, years, col, ref)
}
\arguments{
\item{dt}{Dataset consisting all years}

\item{years}{All years in the dataset}

\item{col}{Selected column to be controlled for implicit null}

\item{ref}{Reference values for the selected column}
}
\description{
Find implicit null categories for selected columns on every selected year
}
\seealso{
Other implicit-null functions: 
\code{\link{do_implicit_null}()},
\code{\link{find_implicit_null}()},
\code{\link{get_implicit_col}()},
\code{\link{get_implicit_per_year}()},
\code{\link{get_implicit_ref}()}
}
\concept{implicit-null functions}
