% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{ident}
\alias{ident}
\title{Returns the argument.}
\usage{
ident(x, ...)
}
\arguments{
\item{x}{The argument to return (unmodified).}
}
\description{
This function returns the passed first argument and ignores any additional arguments.
If the first argument (\code{x}) is unspecified, \code{NULL} is the default.
}
\details{
This is simply a wrapper around \code{base::identity}, but because it specifies defaults and takes additional arguments, is a bit safer/easier to use in function delegators, like \code{base::do.call} or \code{rlang::exec}.
}
