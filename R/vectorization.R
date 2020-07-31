
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title vec
#'
#' @description Vectorization.
#'
#' @details \deqn{
#'   \mathrm{vec} \left( \mathbf{A} \right)
#'   = \left[a_{1, 1}, \cdots, a_{m, 1}, a_{1, 2}, \cdots, a_{m, 2}, \cdots a_{1, n}, \cdots, a_{m, n} \right]^{T}
#'   }
#' @inheritParams is.square
#' @references
#' [Wikipedia: Vectorization](https://en.wikipedia.org/wiki/Vectorization_(mathematics))
#' @examples
#' A <- matrix(
#'   data = c(1, 2, 2, 1),
#'   ncol = 2
#' )
#' vec(A)
#' @export
vec <- function(X) {
  matrix(
    data = X,
    ncol = 1
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title vech
#'
#' @description Half-vectorization.
#'
#' @details \deqn{
#'     \mathrm{vech} \left( \mathbf{A} \right)
#'     = \left[ a_{1, 1}, \cdots, a_{n, 1}, a_{2, 2}, \cdots, a_{n, 2}, \cdots,
#'     a_{n - 1, n - 1}, a_{n, n - 1}, a_{n, n} \right]^{T} .
#'   }
#'
#' @inheritParams low2sym
#' @references
#' [Wikipedia: Half-vectorization](https://en.wikipedia.org/wiki/Vectorization_(mathematics)#Half-vectorization)
#' @examples
#' A <- matrix(
#'   data = c(1, 2, 2, 1),
#'   ncol = 2
#' )
#' vec(A)
#' vech(A)
#' @export
vech <- function(X) {
  is.square(
    X = X,
    stop = TRUE
  )
  matrix(
    data = X[lower.tri(X, diag = TRUE)],
    ncol = 1
  )
}
