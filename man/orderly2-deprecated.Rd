% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated.R, R/location.R
\name{orderly2-deprecated}
\alias{orderly2-deprecated}
\alias{orderly_location_pull_metadata}
\alias{orderly_location_pull_packet}
\title{Deprecated functions in orderly2. These functions still work (\emph{for
now}) but will be removed shortly.  Please update your code.}
\usage{
orderly_location_pull_metadata(...)

orderly_location_pull_packet(...)
}
\arguments{
\item{...}{Arguments forwarded to the new version}
}
\value{
See the linked pages above for the current return type.
}
\description{
\itemize{
\item \code{orderly_location_pull_packet}; please use \code{\link[=orderly_location_pull]{orderly_location_pull()}}
\item \code{orderly_location_pull_metadata}; please use
\code{\link[=orderly_location_fetch_metadata]{orderly_location_fetch_metadata()}}
}
}
\keyword{internal}
