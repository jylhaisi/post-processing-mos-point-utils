% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_useful.R
\name{ifrm}
\alias{ifrm}
\title{Small helper function to remove selected variables in a workspace.}
\usage{
ifrm("variable")
}
\arguments{
\item{x}{The variable which is removed}
}
\description{
This function removes variables in the global workspace, if they are present in it. If they are not, the function does not return error.
}
