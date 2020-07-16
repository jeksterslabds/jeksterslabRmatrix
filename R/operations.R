#' Trace of a square matrix
#'
#' @description Sum of the diagonal of a square matrix.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams is.symmetric
#' @return
#'   Returns the sum of the diagonal of a square matrix.
#' @family matrix functions
#' @keywords matrix
#' @examples
#' Sigma <- matrix(
#'   data = c(
#'     225, 112.50, 56.25,
#'     112.5, 225, 112.5,
#'     56.25, 112.50, 225
#'   ),
#'   ncol = 3
#' )
#' tr(X = Sigma)
#' @references
#'   [Wikipedia: Trace (Linear Algebra)](https://en.wikipedia.org/wiki/Trace_(linear_algebra))
#' @export
tr <- function(X) {
  square <- is.square(
    X = X,
    stop = TRUE
  )
  sum(diag(X))
}
