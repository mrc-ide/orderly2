% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata.R
\name{orderly_shared_resource}
\alias{orderly_shared_resource}
\title{Copy shared resources into a packet directory}
\usage{
orderly_shared_resource(...)
}
\arguments{
\item{...}{The shared resources to copy. If arguments are named, the name
will be the destination file while the value is the filename within the
shared resource directory.

You can use a limited form of string interpolation in the names of
this argument; using \verb{$\{variable\}} will pick up values from
\code{envir} and substitute them into your string.  This is similar
to the interpolation you might be familiar with from
\code{glue::glue} or similar, but much simpler with no concatenation
or other fancy features supported.}
}
\value{
Invisibly, a data.frame with columns \code{here} (the fileames
as as copied into the running packet) and \code{there} (the filenames
within \verb{shared/}).  As for \link{orderly_resource}, do not
rely on the ordering where directory expansion was performed.
}
\description{
Copy shared resources into a packet directory. You can use this to
share common resources (data or code) between multiple packets.
Additional metadata will be added to keep track of where the files
came from.  Using this function requires the shared resources
directory \verb{shared/} exists at the orderly root; an error will be
raised if this is not configured when we attempt to fetch files.
}
