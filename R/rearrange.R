#' Reindex a Symmetric Matrix.
#'
#' @description Reindex a symmetric matrix.
#'
#' @param X Numeric matrix.
#'   Symmetric matrix.
#' @param index Vector of integers.
#'   Index used to reindex the `X`.
#' @return
#'   Returns a reindexed symmetric matrix.
#' @references
#'   [Wikipedia: Permutation Matrix](https://en.wikipedia.org/wiki/Permutation_matrix)
#' @export
reindex <- function(X,
                    index) {
  is.symmetric(
    X = X,
    stop = TRUE
  )
  if (length(index) != nrow(X)) {
    stop(
      "\'index\' and \'nrow(X)\' should be equal."
    )
  }
  P <- matrix(
    data = 0,
    nrow = nrow(X),
    ncol = ncol(X)
  )
  for (i in 1:nrow(P)) {
    P[i, index[i]] <- 1
  }
  # P is the permutation matrix
  out <- P %*% X
  out[, index]
}
