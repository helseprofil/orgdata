% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-multi-input.R
\name{find_column_multi}
\alias{find_column_multi}
\alias{find_column_multi_input}
\alias{find_column_multi_input_arg}
\title{Columns with Multiple Inputs}
\usage{
find_column_multi(spec = NULL, col = NULL, sep = c(",", "|", ":", ";", "&"))

find_column_multi_input(input = NULL)

find_column_multi_input_arg(input = NULL, arg = NULL)
}
\arguments{
\item{spec}{Specifications data as a data.frame}

\item{col}{Column name in the database table}

\item{sep}{Symbols that separate these arguments eg. "," or ":"}

\item{input}{Input argument(s) as a character vector}

\item{arg}{Name of a specific argument in the column}
}
\value{
Output:
\itemize{
\item{\code{find_column_multi} gives a character vector of the
arguments that is separated with \code{sep} argument}
\item{\code{find_column_multi_input} gives a list of argument names and their values}
\item{\code{find_column_multi_input_arg} gives a single object with value from the selected argument}
}
}
\description{
Get all arguments in a selected column that have multiple
arguments with \code{\link[=find_column_multi]{find_column_multi()}}. The output will be a character
type of arguments length. This can then can be used in \code{\link[=find_column_multi_input]{find_column_multi_input()}}
function to get the value of the input as a list object. If you are only
interested in a specific argument among these arguments in the column,
then use \code{\link[=find_column_multi_input_arg]{find_column_multi_input_arg()}} function. See example.
}
\examples{
\dontrun{
args <- find_column_multi(spec, "INNLESARG")
vals <- find_column_multi_input(args)
val <- find_column_multi_input_arg(args, "header")
}
}
\seealso{
Other input-argument functions: 
\code{\link{find_column_input}()}

Other input-argument functions: 
\code{\link{find_column_input}()}

Other input-argument functions: 
\code{\link{find_column_input}()}
}
\concept{input-argument functions}
