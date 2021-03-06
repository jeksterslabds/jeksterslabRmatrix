#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Square Matrix
#'
#' @description Checks if a matrix is square \eqn{\left( A_{m \times m} \right)} .
#'
#' @param X Numeric matrix.
#' @param stop Logical.
#'   If `TRUE`,
#'   stops and returns an error if `X` is not a square matrix.
#'   If `FALSE`,
#'   returns the result of the test.
#' @return
#'   If `stop = FALSE`,
#'   returns `TRUE`,
#'   if the number of row and columns in `X` are equal, or
#'   returns `FALSE`,
#'   if the number of row and columns in `X` are not equal.
#' @examples
#' A <- matrix(
#'   data = 1:9,
#'   nrow = 3
#' )
#' dim(A)
#' B <- matrix(
#'   data = 1:10,
#'   ncol = 2
#' )
#' dim(B)
#' # Returns TRUE
#' is.square(X = B)
#' # Returns FALSE
#' is.square(X = A)
#' @references
#' [Wikipedia: Square matrix](https://en.wikipedia.org/wiki/Square_matrix)
#' @export
is.square <- function(X,
                      stop = FALSE) {
  test <- nrow(X) == ncol(X)
  if (!test) {
    if (stop) {
      stop(
        "\'X\' is not a square matrix."
      )
    }
  }
  test
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Diagonal Matrix
#'
#' @description Checks if off-diagonal elements of a square matrix are all zeroes.
#'
#' @details The off-diagonal elements are added and compared to a tolerance value.
#'   If the sum is less than or equal to the tolerance value,
#'   all the elements are assumed to be zeroes.
#'
#' @inheritParams is.square
#' @param X Square numeric matrix.
#' @param tol Numeric.
#'   Tolerance.
#' @param stop Logical.
#'   If `TRUE`,
#'   stops and returns an error if `X` is not a diagonal matrix.
#'   If `FALSE`,
#'   returns the result of the test.
#' @return
#'   If `stop = FALSE`,
#'   returns `TRUE`,
#'   if the sum of the off-diagonal elements of `X` is equal to zero, or
#'   returns `FALSE`,
#'   if the sum of the off-diagonal elements of `X` is NOT equal to zero.
#' @examples
#' A <- matrix(
#'   data = 1:9,
#'   nrow = 3
#' )
#' B <- matrix(
#'   data = 1:10,
#'   ncol = 2
#' )
#' C <- diag(3)
#' # Returns TRUE
#' is.diag(X = C)
#' # Returns FALSE
#' is.diag(X = A)
#' is.diag(X = B)
#' @references
#' [Wikipedia: Diagonal matrix](https://en.wikipedia.org/wiki/Diagonal_matrix)
#' @export
is.diag <- function(X,
                    tol = 1e-8,
                    stop = FALSE) {
  test.square <- is.square(
    X = X,
    stop = stop
  )
  if (!test.square) {
    return(FALSE)
  } else {
    diag(X) <- rep(
      x = 0,
      length = nrow(X)
    )
    test <- sum(as.vector(X)) <= tol
    if (!test) {
      if (stop) {
        stop(
          "\'X\' is not a diagonal matrix."
        )
      }
    }
  }
  test
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Symmetric Matrix
#'
#' @description Checks if a matrix is symmetric.
#'
#' @param X Numeric matrix.
#'   A \eqn{p \times p} matrix.
#' @param stop Logical.
#'   If `TRUE`,
#'   stops and returns an error
#'   with any of the following conditions:
#'   `X` is not a square matrix or
#'   `X` is not a symmetric matrix.
#' @return
#'   Returns `FALSE`,
#'   if the `X` is not square and not symmetric.
#'   Returns `TRUE`,
#'   if the `X` is symmetric.
#' @examples
#' Sigma <- matrix(
#'   data = c(
#'     225, 112.50, 56.25,
#'     112.5, 225, 112.5,
#'     56.25, 112.50, 225
#'   ),
#'   ncol = 3
#' )
#' is.symmetric(X = Sigma)
#' @export
is.symmetric <- function(X,
                         stop = FALSE) {
  square <- is.square(
    X = X,
    stop = stop
  )
  if (!square) {
    test <- FALSE
  } else {
    test <- sum(X == t(X)) == (nrow(X)^2)
  }
  if (!test) {
    if (stop) {
      stop(
        "\'X\' is not a symmetric matrix."
      )
    }
  }
  test
}

## Add lower triangular and upper triangular in the future

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Positive Definite
#'
#' @description Checks if eigenvalues in a square matrix are positive.
#'
#' @inheritParams is.diag
#' @param X Numeric matrix.
#'   Symmetric matrix.
#' @param stop Logical.
#'   If `TRUE`,
#'   stops and returns an error
#'   with any of the following conditions:
#'   `X` is not a square matrix,
#'   `X` is not a symmetric matrix, or
#'   `X` is not a positive definite matrix.
#' @return
#'   Returns `TRUE`
#'   if the matrix is positive definite,
#'   that is,
#'   all eigenvalues are positive.
#'   Returns `FALSE`
#'   if the matrix is not positive definite,
#'   that is,
#'   any of the eigenvalues are are less than or equal to zero.
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
#' is.positive.definite(X = Sigma)
#' @references
#'   [Wikipedia: Definiteness of a Matrix](https://en.wikipedia.org/wiki/Definiteness_of_a_matrix)
#' @export
is.positive.definite <- function(X,
                                 tol = 1e-8,
                                 stop = FALSE) {
  symmetric <- is.symmetric(
    X = X,
    stop = stop
  )
  if (!symmetric) {
    test <- FALSE
  } else {
    eigenvalues <- eigen(x = X, only.values = TRUE)$values
    p <- nrow(X)
    for (i in 1:p) {
      if (abs(eigenvalues[i] < tol)) {
        eigenvalues[i] <- 0
      }
    }
    if (any(eigenvalues <= 0)) {
      test <- FALSE
    } else {
      test <- TRUE
    }
  }
  if (!test) {
    if (stop) {
      stop(
        "\'X\' is not a positive definite matrix."
      )
    }
  }
  test
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Invertible Matrix
#'
#' @description Checks if a square matrix is invertible.
#'
#' @param X Numeric matrix.
#'   Square matrix.
#' @param stop Logical.
#'   If `TRUE`,
#'   stops and returns an error
#'   with any of the following conditions:
#'   `X` is not a square matrix, or
#'   `X` is not invertible, that is, a singular matrix.
#' @inheritParams is.positive.definite
#' @return
#'   Returns `TRUE`
#'   if the matrix is invertible.
#'   Returns `FALSE`
#'   if the matrix is not invertible, that is, singular.
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
#' is.invertible(X = Sigma)
#' @references
#'   [Wikipedia: Invertible Matrix](https://en.wikipedia.org/wiki/Invertible_matrix)
#'   [Wikipedia: Singular Matrix](https://en.wikipedia.org/wiki/Singular_matrix)
#' @export
is.invertible <- function(X,
                          tol = 1e-8,
                          stop = FALSE) {
  square <- is.square(
    X = X,
    stop = stop
  )
  if (!square) {
    test <- FALSE
  } else {
    if (det(X) < tol) {
      # singular
      # not invertible
      test <- FALSE
    } else {
      # invertible
      test <- TRUE
    }
  }
  if (!test) {
    if (stop) {
      stop(
        "\'X\' is not invertible."
      )
    }
  }
  test
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Singular Matrix
#'
#' @description Checks if a square matrix is non invertible or singular.
#'
#' @param stop Logical.
#'   If `TRUE`,
#'   stops and returns an error if `X` is not a square matrix.
#' @inheritParams is.invertible
#' @return
#'   Returns `TRUE`
#'   if the matrix is not invertible, that is, singular.
#'   Returns `FALSE`
#'   if the matrix is invertible.
#' @inherit is.invertible references
#' @export
is.singular <- function(X,
                        tol = 1e-8,
                        stop = FALSE) {
  square <- is.square(
    X = X,
    stop = stop
  )
  out <- is.invertible(
    X = X,
    tol = tol,
    stop = FALSE
  )
  if (out) {
    test <- FALSE
  } else {
    test <- TRUE
  }
  test
}
