% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata.R
\name{orderly_resource}
\alias{orderly_resource}
\title{Declare orderly resources}
\usage{
orderly_resource(files)
}
\arguments{
\item{files}{Any number of names of files or directories.  If you
list a directory it is expanded recursively to include all
subdirectories and files.}
}
\value{
Invisibly, a character vector of resources included by the
call. Don't rely on the order of these files if they are
expanded from directories, as this is likely platform dependent.
}
\description{
Declare that a file, or group of files, are an orderly
resource. By explicitly declaring files as resources orderly will
mark the files as immutable inputs and validate that your analysis
does not modify them when run with \code{\link[=orderly_run]{orderly_run()}}
}
