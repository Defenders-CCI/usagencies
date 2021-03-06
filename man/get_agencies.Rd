% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_agencies}
\alias{get_agencies}
\title{Get agencies matching a query string}
\usage{
get_agencies(query)
}
\arguments{
\item{query}{Query string for the branch/department/agency of interest}
}
\value{
A data.frame of rows matching the query
}
\description{
Get agencies matching a query string using approximate pattern matching
(agrep).
}
\examples{
get_agencies("Fish and Wildlife")
}

