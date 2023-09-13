.handle_trim <- function(x, trim) {
  n <- length(x)
  if (trim > 0 && n > 0) {
    if (is.complex(x)) {
      stop("trimmed means are not defined for complex data")
    }
    if (trim >= 0.5) {
      return(stats::median(x, na.rm = FALSE))
    }
    lo <- floor(n * trim) + 1
    hi <- n + 1 - lo
    x <- sort.int(x, partial = unique(c(lo, hi)))[lo:hi]
  }
  return(x)
}
