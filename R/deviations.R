mat_dev <- function(X) {
  n <- nrow(X)
  ones <- rep(x = 1, times = n)
  X - tcrossprod(ones) %*% X * (1 / n)
  # deviation
  # scale(X, center = TRUE, scale = FALSE)
}

mat_cov <- function(X) {
  X_centered <- scale(X, center = TRUE, scale = FALSE)
  crossprod(X_centered) * (1 / (nrow(X) - 1))
}

mat_cor <- function(X) {
  X_scaled <- scale(X, center = TRUE, scale = TRUE)
  crossprod(X_scaled) * (1 / (nrow(X) - 1))
}
