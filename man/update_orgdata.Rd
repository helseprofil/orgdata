% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-misc.R
\name{update_orgdata}
\alias{update_orgdata}
\alias{upgrade_orgdata}
\title{Update package}
\usage{
update_orgdata(branch = c("main", "dev"), force = FALSE)

upgrade_orgdata(branch = c("main", "dev"), force = FALSE)
}
\arguments{
\item{branch}{The branch in Github to install from}

\item{force}{Use the laterst version(s) of all dependencies. Default is FALSE}
}
\description{
Update orgdata directly with Github repo. Default is \code{main} branch.
}
\examples{
\dontrun{
update_orgdata(branch = "main") #default
update_orgdata(branch = "dev") #to upgrade with dev branch
}
}
