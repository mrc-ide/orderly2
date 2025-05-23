% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gitignore.R
\name{orderly_gitignore_update}
\alias{orderly_gitignore_update}
\title{Update a gitignore file}
\usage{
orderly_gitignore_update(name, root = NULL)
}
\arguments{
\item{name}{The name of the gitignore file to update, or the
string "(root)"}

\item{root}{The path to the root directory, or \code{NULL} (the
default) to search for one from the current working
directory. This function \strong{does} require that the directory is
configured for orderly, and not just outpack (see
\link{orderly_init} for details).}
}
\value{
Nothing, called for its side effects
}
\description{
Update a gitignore, which is useful to prevent accidentally
committing files to source control that are generated. This
includes artefacts, shared resources and dependencies (within a
report directory) or at the global level all the contents of the
\code{.outpack} directory, the draft folder and the archive directory.
}
\details{
If this function fails with a message \verb{Can't edit '.gitignore', markers are corrupted}, then look for the special markers within
the \code{.gitignore} file.  It should look like

\if{html}{\out{<div class="sourceCode">}}\preformatted{# ---VVV--- added by orderly ---VVV----------------
# Don't manually edit content between these markers
... patterns
# ---^^^--- added by orderly ---^^^----------------
}\if{html}{\out{</div>}}

We can't edit the file if:
\itemize{
\item any of these lines appears more than once in the file
\item there is anything between the first two lines
\item they are not in this order
}

If you get the error message, search and remove these lines and
rerun.
}
