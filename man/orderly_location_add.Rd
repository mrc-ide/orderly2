% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/location.R
\name{orderly_location_add}
\alias{orderly_location_add}
\alias{orderly_location_add_path}
\alias{orderly_location_add_http}
\alias{orderly_location_add_packit}
\title{Add a new location}
\usage{
orderly_location_add(name, type, args, verify = TRUE, root = NULL)

orderly_location_add_path(name, path, verify = TRUE, root = NULL)

orderly_location_add_http(name, url, verify = TRUE, root = NULL)

orderly_location_add_packit(
  name,
  url,
  token = NULL,
  save_token = NULL,
  verify = TRUE,
  root = NULL
)
}
\arguments{
\item{name}{The short name of the location to use.  Cannot be in
use, and cannot be one of \code{local} or \code{orphan}}

\item{type}{The type of location to add. Currently supported
values are \code{path} (a location that exists elsewhere on the
filesystem) and \code{http} (a location accessed over outpack's http
API).}

\item{args}{Arguments to the location driver. The arguments here
will vary depending on the type used, see Details.}

\item{verify}{Logical, indicating if we should verify that the
location can be used before adding.}

\item{root}{The path to the root directory, or \code{NULL} (the
default) to search for one from the current working
directory. This function does not require that the directory is
configured for orderly, and can be any \code{outpack} root (see
\link{orderly_init} for details).}

\item{path}{The path to the other archive root. This can be a
relative or absolute path, with different tradeoffs.  If you use
an absolute path, then this location will typically work well on
this machine, but it may behave poorly when the location is
found on a shared drive \strong{and} when you use your orderly root
from more than one system.  This setup is common when using an
HPC system.  If you use a relative path, then we will interpret
it \strong{relative to your orderly root} and not the directory that
you evaluate this command from.  Typically your path should
include leading dots (e.g. \verb{../../somewhere/else}) as you should
not nest orderly projects.  This approach should work fine on
shared filesystems.}

\item{url}{The location of the server, including protocol, for
example \verb{http://example.com:8080}}

\item{token}{The value for your your login token (currently this
is a GitHub token with \code{read:org} scope). If \code{NULL}, orderly2
will perform an interactive authentication against GitHub to
obtain one.}

\item{save_token}{If no token is provided and interactive
authentication is used, this controls whether the GitHub token
should be saved to disk.  Defaults to \code{TRUE} if \code{NULL}.}
}
\value{
Nothing
}
\description{
Add a new location - a place where other packets might be found
and pulled into your local archive.  Currently only file and http
based locations are supported, with limited support for custom
locations. Note that adding a location does \emph{not} pull metadata
from it, you need to call
\link{orderly_location_fetch_metadata} first.  The function
\code{orderly_location_add} can add any sort of location, but the other
functions documented here (\code{orderly_location_add_path}, etc) will
typically be much easier to use in practice.
}
\details{
We currently support three types of locations - \code{path}, which points
to an outpack archive accessible by path (e.g., on the same
computer or on a mounted network share), \code{http}, which requires
that an outpack server is running at some url and uses an HTTP API
to communicate, and \code{packit}, which uses Packit as a web
server.  More types may be added later, and more configuration
options to these location types will definitely be needed in
future.

Configuration options for different location types are described
in the arguments to their higher-level functions.

\strong{Path locations}:

Use \code{orderly_location_add_path}, which accepts a \code{path} argument.

\strong{HTTP locations}:

Accessing outpack over HTTP requires that an outpack server is
running. The interface here is expected to change as we expand
the API, but also as we move to support things like TLS and
authentication.

Use \code{orderly_location_add_http}, which accepts a \code{url} argument.

\strong{Packit locations}:

Packit locations work over HTTPS, and include everything in an
outpack location but also provide authentication and later will
have more capabilities we think.

Use \code{orderly_location_add_packit}, which accepts \code{url}, \code{token}
and \code{save_token} arguments.

\strong{Custom locations}:

All outpack implementations are expected to support path and http
locations, with the standard arguments above.  But we expect that
some implementations will support custom locations, and that the
argument lists for these may vary between implementations. To
allow this, you can pass a location of type "custom" with a list
of arguments.  We expect an argument 'driver' to be present among
this list.  For an example of this in action, see the
\href{https://mrc-ide.github.io/outpack.sharepoint}{\code{outpack.sharepoint}}
package.

\emph{Be warned that we may change this interface in future, in which
case you may need to update your configuration.}
}
\section{Warning}{


The API here may change as we move to support different types of
locations.
}

