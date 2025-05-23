% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query_explain.R
\name{orderly_query_explain}
\alias{orderly_query_explain}
\title{Explain a query}
\usage{
orderly_query_explain(
  expr,
  name = NULL,
  scope = NULL,
  subquery = NULL,
  parameters = NULL,
  envir = parent.frame(),
  location = NULL,
  allow_remote = NULL,
  root = NULL
)
}
\arguments{
\item{expr}{The query expression. A \code{NULL} expression matches everything.}

\item{name}{Optionally, the name of the packet to scope the query on. This
will be intersected with \code{scope} arg and is a shorthand way of running
\code{scope = list(name = "name")}}

\item{scope}{Optionally, a scope query to limit the packets
searched by \code{pars}}

\item{subquery}{Optionally, named list of subqueries which can be
referenced by name from the \code{expr}.}

\item{parameters}{Optionally, a named list of parameters to substitute
into the query (using the \verb{this:} prefix)}

\item{envir}{Optionally, an environment to substitute into the
query (using the \verb{environment:} prefix). The default here is to
use the calling environment, but you can explicitly pass this in
if you want to control where this lookup happens.}

\item{location}{Optional vector of locations to pull from. We
might in future expand this to allow wildcards or exceptions.}

\item{allow_remote}{Logical, indicating if we should allow packets
to be found that are not currently unpacked (i.e., are known
only to a location that we have metadata from). If this is
\code{TRUE}, then in conjunction with \link{orderly_dependency}
you might pull a large quantity of data.  The default is \code{NULL}. This is
\code{TRUE} if remote locations are listed explicitly as a character
vector in the \code{location} argument, or if you have specified
\code{fetch_metadata = TRUE}, otherwise \code{FALSE}.}

\item{root}{The path to the root directory, or \code{NULL} (the
default) to search for one from the current working
directory. This function does not require that the directory is
configured for orderly, and can be any \code{outpack} root (see
\link{orderly_init} for details).}
}
\value{
An object of class \code{orderly_query_explain}, which can be
inspected (contents subject to change) and which has a print
method which will show a user-friendly summary of the query
result.
}
\description{
Explain how a query has or has not matched. This is experimental
and the output will change. At the moment, it can tell you why a
query matches, or if fails to match based on one of a number of
\code{&&}-ed together clauses.
}
