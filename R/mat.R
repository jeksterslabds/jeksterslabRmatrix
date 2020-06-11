#' Square Matrix
#'
#' Checks if a matrix is square.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param X Matrix.
#' @return
#'   Returns `TRUE`,
#'   if the number of row and columns in `X` are equal.
#'   Returns `FALSE`,
#'   if the number of row and columns in `X` are not equal.
#' @export
is.square <- function(X) {
  if (nrow(X) == ncol(X)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Positive Definite
#'
#' Checks if eigenvalues in a square matrix are positive.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param X Numeric matrix.
#'   A \eqn{p \times p} matrix.
#' @param tol Numeric.
#'   Tolerance.
#' @return
#'   Returns `FALSE`
#'   if any of the eigenvalues are less than or equal to 0.
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
                                 tol = 1e-8) {
  if (!is.square(X)) {
    stop(
      "\'X'\ is not a square matrix."
    )
  }
  eigenvalues <- eigen(x = X, only.values = TRUE)$values
  p <- nrow(X)
  for (i in 1:p) {
    if (abs(eigenvalues[i] < tol)) {
      eigenvalues[i] <- 0
    }
  }
  if (any(eigenvalues <= 0)) {
    return(FALSE)
  }
  return(TRUE)
}

#' Singular Matrix
#'
#' Checks if a square matrix is not invertible.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams is.positive.definite
#' @return
#'   Returns `TRUE`
#'   if the matrix is singular or not invertible,
#'   meaning the determinant of the matrix is 0.
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
#' is.singular.matrix(X = Sigma)
#' @references
#'   [Wikipedia: Singular Matrix](https://en.wikipedia.org/wiki/Singular_matrix)
#' @export
is.singular.matrix <- function(X,
                               tol = 1e-8) {
  det(X) < tol
}

#' Correlation to Covariance
#'
#' Converts a correlation matrix to a covariance matrix.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param cor Numeric matrix.
#'   A \eqn{p \times p} positive definite correlation matrix.
#' @param sd Numeric vector.
#'   A vector of \eqn{p} standard deviations.
#' @return
#'   Returns a covariance matrix.
#' @family matrix functions
#' @keywords matrix
#' @examples
#' R <- matrix(
#'   data = c(
#'     1, 0.509902, 0.26,
#'     0.509902, 1, 0.509902,
#'     0.26, 0.509902, 1
#'   ),
#'   ncol = 3
#' )
#' cor2cov(
#'   cor = R,
#'   sd = c(15, 15, 15)
#' )
#' @export
cor2cov <- function(cor,
                    sd) {
  if (!is.square(cor)) {
    stop(
      "\'cor'\ is not a square matrix."
    )
  }
  cor * tcrossprod(sd)
}

#' Trace of a square matrix
#'
#' Sum of the diagonal of a square matrix.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams is.positive.definite
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
  if (!is.square(X)) {
    stop(
      "\'X'\ is not a square matrix."
    )
  }
  sum(diag(X))
}

#' Lower Triangle to Symmetric
#'
#' Creates a symmetric matrix from the lower triangle
#'   of a square matrix.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams is.positive.definite
#' @return Returns a symmetric matrix.
#' @family matrix functions
#' @keywords matrix
#' @examples
#' X <- matrix(NA, ncol = 4, nrow = 4)
#' X[lower.tri(X, diag = TRUE)] <- 1:10
#' low2sym(X = X)
#' @export
low2sym <- function(X) {
  if (!is.square(X)) {
    stop(
      "\'X'\ is not a square matrix."
    )
  }
  X[upper.tri(X)] <- t(X)[upper.tri(X)]
  X
}

#' Upper Triangle to Symmetric
#'
#' Creates a symmetric matrix from the upper triangle
#'   of a square matrix.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams is.positive.definite
#' @return Returns a symmetric matrix.
#' @family matrix functions
#' @keywords matrix
#' @examples
#' X <- matrix(NA, ncol = 4, nrow = 4)
#' X[upper.tri(X, diag = TRUE)] <- 1:10
#' up2sym(X = X)
#' @export
up2sym <- function(X) {
  X[lower.tri(X)] <- t(X)[lower.tri(X)]
  X
}

#' Vector to Triangular Matrix
#'
#' Fills a matrix with a vector that represents the triangular elements.
#'   The function can produce a strictly triangular matrix,
#'   that is, the diagonal elements are set to zero
#'   or a triangular matrix with diagonal values
#'   supplied by the argument `x`.
#'   Note that the matrix is filled by column.
#' @author Ivan Jacob Agaloos Pesigan
#' @param x A vector.
#' @param lower Logical.
#'   If `TRUE`, creates a lower triangular matrix.
#'   If `FALSE`, creates an upper triangular matrix.
#' @param diag Logical.
#'   If `TRUE`,
#'   values of the diagonal as supplied by `x`.
#'   If `FALSE`,
#'   diagonals are set to zero and
#'   `x` fills only the off-diagonal elements of the matrix
#'   producing a strictly triangular matrix.
#' @examples
#' # Stricly lower triangular matrix
#' vech2tri(1:10, lower = TRUE, diag = FALSE)
#' # Lower triangular matrix with diagonal and off-diagonal values
#' # supplied by the argument x
#' vech2tri(1:10, lower = TRUE, diag = TRUE)
#' # Stricly upper triangular matrix
#' vech2tri(1:10, lower = FALSE, diag = FALSE)
#' # Upper triangular matrix with diagonal and off-diagonal values
#' # supplied by the argument x
#' vech2tri(1:10, lower = FALSE, diag = TRUE)
#' @export
vech2tri <- function(x, lower = TRUE, diag = FALSE) {
  if (length(x) == 1) {
    p <- 2
  } else {
    if (diag) {
      p <- (sqrt(1 + 8 * length(x)) - 1) / 2
    } else {
      p <- (sqrt(1 + 8 * length(x)) + 1) / 2
    }
    if (!as.integer(p) == p) {
      stop("\"x\" must have the correct number of elements to fill the off-diagonal elements of the matrix")
    }
  }
  A <- matrix(
    data = 0,
    nrow = p,
    ncol = p
  )
  if (lower) {
    i <- which(x = lower.tri(x = A, diag = diag), arr.ind = TRUE)
  } else {
    i <- which(x = upper.tri(x = A, diag = diag), arr.ind = TRUE)
  }
  A[i] <- x
  A
}
