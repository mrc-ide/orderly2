% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plugin.R
\name{orderly_plugin_context}
\alias{orderly_plugin_context}
\title{Fetch plugin context}
\usage{
orderly_plugin_context(name, envir)
}
\arguments{
\item{name}{Name of the plugin}

\item{envir}{The environment of the calling function. You can
typically pass \code{parent.frame()} (or \code{rlang::caller_env()}) here
if the function calling \code{orderly_plugin_context()} is the
function that would be called by a user. This argument only has
an effect in interactive use (where \code{envir} is almost certainly
the global environment).}
}
\value{
A list with elements:
\itemize{
\item \code{is_active}: a logical, indicating if we're running under
\code{\link[=orderly_run]{orderly_run()}}; you may need to change behaviour
depending on this value.
\item \code{path}: the path of the running packet. This is almost always the
working directory, unless the packet contains calls to \code{\link[=setwd]{setwd()}}
or similar. You may create files here.
\item \code{config}: the configuration for this plugin, after processing
with the plugin's \code{read} function (see
\code{\link{orderly_plugin_register}})
\item \code{envir}: the environment that the packet is running in. Often this
will be the global environment, but do not assume this! You may
read and write from this environment.
\item \code{src}: the path to the packet source directory. This is
different to the current directory when the packet is running,
but the same when the user is interactively working with a
report. You may \emph{read} from this directory but \emph{must not write
to it}
\item \code{parameters}: the parameters as passed through to the run the
report.
}
}
\description{
Fetch the running context, for use within a plugin.  The intention
here is that within free functions that your plugin makes
available, you will call this function to get information about
the state of a packet.  You will then typically call
\code{\link[=orderly_plugin_add_metadata]{orderly_plugin_add_metadata()}} afterwards.
}
\details{
When a plugin function is called, orderly2 will be running in one
of two modes; (1) from within \code{\link[=orderly_run]{orderly_run()}}, in
which case we're part way through creating a packet in a brand new
directory, and possibly using a special environment for
evaluation, or (2) interactively, with a user developing their
report. The plugin needs to be able to support both modes, and
this function will return information about the state to help you
cope with either case.
}
\seealso{
\link{orderly_plugin_register},
\link{orderly_plugin_add_metadata}
}
