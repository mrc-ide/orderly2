% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{orderly_parse_file}
\alias{orderly_parse_file}
\alias{orderly_parse_expr}
\title{Parse the orderly entrypoint script}
\usage{
orderly_parse_file(path)

orderly_parse_expr(exprs, filename)
}
\arguments{
\item{path}{Path to \verb{orderly_*} script}

\item{exprs}{Parsed AST from \verb{orderly_*} script}

\item{filename}{Name of \verb{orderly_*} file to include in metadata}
}
\value{
Parsed orderly entrypoint script
}
\description{
For expert use only.
}
\details{
Parses details of any calls to the orderly_ in-script functions
into intermediate representation for downstream use. Also validates
that any calls to \verb{orderly_*} in-script functions are well-formed.
}
