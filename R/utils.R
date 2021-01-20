#' @title Returns an element suitable for inserting into a column of a zero-row dataframe.
#'
#' @description
#' * If `x` is a dataframe itself, the zero-row version of that dataframe is returned.
#' * If `x` is a vector, then `x[0]` is returned.
#' * If `x` is something else, then `x[0]` is returned, possibly unsafely.
#'
#' @param x The value for which you desire a zero-length version.
.zerorow <- function(x) {
  if (is.data.frame(x)) {
    x[0,]
  } else if (rlang::is_vector(x)) {
    x[0]
  } else {
    x[0]  ## fallback to just trying here
  }
}


#' @title Returns the argument.
#'
#' @description
#' This function returns the passed first argument and ignores any additional arguments.
#' If the first argument (`x`) is unspecified, `NULL` is the default.
#'
#' @details
#' This is simply a wrapper around `base::identity`, but because it specifies defaults and takes additional arguments, is a bit safer/easier to use in function delegators, like `base::do.call` or `rlang::exec`.
#'
#' @param x The argument to return (unmodified).
#'
#' @export
ident <- function(x, ...) {
  if (missing(x)) NULL else identity(x)
}


#' @title Prepend classname(s) to an object's S3 class attribute.
#'
#' @param obj an single object.
#' @param klass a character vector of classnames, ordered appropriately (following S3 OO semantics).
#'
#' @return the object with \code{klass} classnames prepended to the \code{class} attribute of \code{obj}.
#'
#' @export
prepend_class <- function(obj, klass) {
  klass <- unique(as.character(klass))
  class(obj) <- append(klass, class(obj))
  obj
}
