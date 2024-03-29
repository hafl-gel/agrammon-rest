% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post_function.r
\name{agrammon_defaults}
\alias{agrammon_defaults}
\title{Set Agrammon Options}
\usage{
agrammon_defaults(report, filter, language, data.table)
}
\arguments{
\item{report}{set the default for argument \code{report} in function \code{run_agrammon}.}

\item{filter}{set the default for argument \code{filter} in function \code{run_agrammon}.}

\item{language}{set the language of the template help and units}

\item{data.table}{should \code{run_agrammon} return a \code{data.table} or a \code{data.frame}?}
}
\value{
nothing unless no argument is provided (see Details)
}
\description{
set agrammon package related options
}
\details{
This function is helping to set user specific defaults
on a R session basis. 

If no argument is provided, the current user defined defaults
will be returned. 

User specific defaults can be cleared by assigning a value of \code{NULL}.
}
\examples{
# get current user specific defaults
agrammon_defaults()
# set default to 'full' report
agrammon_defaults(report = 'full')
agrammon_defaults()
# remove it again
agrammon_defaults(report = NULL)
agrammon_defaults()
}
