##' @title You're a \code{mean} one, Mr. Grinch
##' @details For \code{geometric.mean}, by default, any zeroes in \code{x}
##'   causes the geometric mean to be identically 0. This is the default action
##'   for \code{zero.action = "keep"}. The other options are \code{zero.action =
##'   "drop"}, which completely drops the zeroes before calculating the harmonic
##'   mean, and \code{zero.action = "ignore"}, which ignores the zeroes when
##'   computing the product, but includes them in $n$ for the root.
##' @param x: An R object. Currently there are methods for numeric/logical
##'   vectors and date, date-time and time interval objects. Complex vectors are
##'   allowed for ‘trim = 0’, only.
##' @param trim: the fraction (0 to 0.5) of observations to be trimmed from each
##'   end of ‘x’ before the mean is computed. Values of trim outside that range
##'   are taken as the nearest endpoint.
##' @param na.rm: a logical evaluating to \code{TRUE} or \code{FALSE} indicating
##'   whether \code{NA} values should be stripped before the computation
##'   proceeds. Default is \code{FALSE}.
##' @param ... further arguments passed to or from other methods.
##' @param zero.action What action should be taken if there are zeroes in
##'   \code{x}? See Details.
##' @return The requested mean
##' @export
##' @rdname grinch_means
arithmetic.mean <- function(x,
                            trim = 0,
                            na.rm = FALSE,
                            ...) {
  mean(x, trim = trim, na.rm = na.rm, ...)
}

##' @export
##' @rdname grinch_means
geometric.mean <- function(x,
                           trim = 0,
                           na.rm = FALSE,
                           ...,
                           zero.action = c("keep", "drop", "ignore")) {

  x <- .handle_trim(x, trim)
  if (isTRUE(na.rm)) {
    x <- x[!is.na(x)]
  }

  if (any(x < 0, na.rm = TRUE)) {
    # Not defined for negative values
    return(NaN)
  }

  zero.action <- match.arg(zero.action)

  if (zero.action == "drop") {
    x <- x[x != 0]
  }

  if (zero.action == "keep" & any(x == 0, na.rm = TRUE)) {
    return(0)
  }

  if (zero.action == "ignore") {
    return(exp(sum(log(x[x > 0])) / length(x)))
  }

  return(exp(mean(log(x))))
}

##' @export
##' @rdname grinch_means
harmonic.mean <- function(x,
                          trim = 0,
                          na.rm = FALSE,
                          ...) {
  x <- .handle_trim(x, trim)
  if (isTRUE(na.rm)) {
    x <- x[!is.na(x)]
  }

  if (any(x < 1, na.rm = TRUE)) {
    # Only defined for positive values
    return(NaN)
  }

  length(x)/sum(1/x, na.rm = na.rm, ...)
}

##' @rdname grinch_means
##' @export
quadratic.mean <- function(x,
                           trim = 0,
                           na.rm = FALSE,
                           ...) {
  x <- .handle_trim(x, trim)
  if (isTRUE(na.rm)) {
    x <- x[!is.na(x)]
  }

  sqrt((1/length(x))*sum(x^2, ...))
}
