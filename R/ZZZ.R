#' @importFrom tidyselect any_of
NULL

#' null replacement
#'
#' if `a` is null, choose `b`.
#'
#' @param a a possibly null value
#' @param b a value to replace `a` with if `a` is null
#'
#' @examples
#' \dontrun{
#' 1 %||% 2 == 1
#' NULL %||% 2 == 2
#' }
#'
#' @name null-replacement
#' @noRd

# Begin Exclude Linting
`%||%` <- function(a, b) {
  if (is.null(a)) {
    b
  } else {
    a
  }
}
# End Exclude Linting

#' checks if a vector is a date
#'
#' validates a vector is a date by checking to see if it is either a Date or POSIXt class
#'
#' @param x a vector to check
#' @return `TRUE` if `x` is a date, `FALSE` otherwise
#' @noRd
is_date <- function(x) {
  inherits(x, c("Date", "POSIXt"))
}
