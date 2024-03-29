% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata.R
\name{orderly_parameters}
\alias{orderly_parameters}
\title{Declare orderly parameters}
\usage{
orderly_parameters(...)
}
\arguments{
\item{...}{Any number of parameters}
}
\value{
Undefined
}
\description{
Declare orderly parameters. You should only have one call to this
within your file, though this is not enforced! Typically you'd put
it very close to the top, though the order does not really matter.
Parameters are scalar atomic values (e.g. a string, number or
boolean) and defaults must be present literally (i.e., they may
not come from a variable itself). Provide \code{NULL} if you do not
have a default, in which case this parameter will be required.
}
\section{Behaviour in interactive sessions}{


When running interactively (i.e., via \code{source()} or running an
orderly file session by copy/paste or in Rstudio), the
\code{orderly_parameters()} function has different behaviour.

First, we look in the current environment (most likely the global
environment) for values of your parameters - that is, variables
bound to the names of your parameters.  For any parameters that
are not found we will look at the default values and use these
if possible, but if not possible then we will either error or
prompt based on the global option
\code{orderly_interactive_parameters_missing_error}. If this is
\code{TRUE}, then we will ask you to enter a value for the parameters
(strings will need to be entered with quotes).
}

