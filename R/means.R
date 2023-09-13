##' @title You're a \code{mean} one, Mr. Grinch
##' @param x An R object. Supports all type that \code{mean()} supports.
##' @param ... Additional arguments such as \code{trim} or \code{na.rm}
##' @param zero.propagate Logical, default \code{FALSE}. Should 0's be dropped
##'   or propgated?
##' @return The arithmetic mean
##' @export
##' @rdname grinch_means
arithmetic.mean <- function(x, ...) {
  mean(x, ...)
}

##' @export
##' @rdname grinch_means
geometric.mean <- function(x, ..., zero.propagate = FALSE) {
  if (any(x < 0, na.rm = TRUE)) {
    # Not defined for negative values
    return(NaN)
  }

  if (zero.propagate) {
    if (any(x == 0, na.rm = TRUE)) {
      return(0)
    }
    exp(mean(log(x), na.rm = na.rm))
  } else {
    exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
  }
}

##' @export
##' @rdname grinch_means
harmonic.mean <- function(x, ...) {
  exp(mean(log(x, ...), ...), ...)
}

##' @rdname grinch_means
##' @export
quadratic.mean <- function(x, ...) {
  sqrt((1/length(x))*sum(x^2, ...))
}
