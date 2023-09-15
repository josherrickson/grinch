##' @param types Which means are to be calculated. Defaults to only the
##'   arithmetic mean.
##' @param simplify_on_one_type Logical. If \code{TRUE} (default), if a single
##'   mean type is requested, return a vector instead of named list. The default
##'   makes \code{mean(x)} and \code{means(x)} return the same thing.
##' @rdname grinch_means
##' @return \code{means()} returns a named list of the requested means. If only
##'   a single type of mean is requested, it instead returns a vector of length
##'   1. The argument \code{simplify_on_one_type} can turn this simplification
##'   off.
##' @export
means <- function(x,
                  types = c("arithmetic",
                            "geometric",
                            "harmonic",
                            "quadratic",
                            "cubic",
                            "generalized",
                            "lehmer",
                            "quasi_arithmetic"),
                  trim = 0,
                  na.rm = FALSE,
                  ...,
                  simplify_on_one_type = TRUE) {

  if (missing(types)) {
    types <- "arithmetic"
  }
  types <- match.arg(types, several.ok = TRUE)

  results <- list()
  for (t in types) {
    mfunc <- get(paste0(t, ".mean"))
    results[[t]] <- tryCatch(mfunc(x, trim = trim, na.rm = na.rm, ...),
                             error = function(e) e)

  }

  if (simplify_on_one_type && length(results) == 1) {
    return(results[[1]])
  }
  return(results)
}
