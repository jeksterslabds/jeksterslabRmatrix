#' ---
#' title: "Test: is.symmetric"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: is.symmetric}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---
#'
#+ knitr_options, include=FALSE, cache=FALSE
knitr::opts_chunk$set(
  error = TRUE,
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)
#'
#+ setup
library(testthat)
library(jeksterslabRmatrix)
context("Test is.symmetric.")
#'
#' ## Parameters
#'
#+ parameters
matrix.T <- matrix(
  data = c(
    225, 112.50, 56.25,
    112.5, 225, 112.5,
    56.25, 112.50, 225
  ),
  ncol = 3
)
matrix.F1 <- matrix(
  data = 1:10,
  ncol = 2
)
matrix.F2 <- matrix(
  data = c(
    225, 0, 0,
    112.5, 225, 0,
    56.25, 112.50, 225
  ),
  ncol = 3
)
colnames(matrix.T) <- paste0("col", 1:ncol(matrix.T))
rownames(matrix.T) <- paste0("row", 1:nrow(matrix.T))
colnames(matrix.F1) <- paste0("col", 1:ncol(matrix.F1))
rownames(matrix.F1) <- paste0("row", 1:nrow(matrix.F1))
colnames(matrix.F2) <- paste0("col", 1:ncol(matrix.F2))
rownames(matrix.F2) <- paste0("row", 1:nrow(matrix.F2))
knitr::kable(
  x = matrix.F1,
  row.names = FALSE,
  caption = "Non-Square Matrix"
)
knitr::kable(
  x = matrix.F2,
  row.names = FALSE,
  caption = "Square but not Symmetric Matrix"
)
knitr::kable(
  x = matrix.T,
  row.names = FALSE,
  caption = "Square Matrix"
)
#'
#' ## Results
#'
#+ results
knitr::kable(
  x = data.frame(
    Item = c(
      "Non-Square Matrix",
      "Square but not Symmetric Matrix",
      "Square Matrix"
    ),
    Parameter = c(
      "FALSE",
      "FALSE",
      "TRUE"
    ),
    Results = c(
      is.symmetric(matrix.F1),
      is.symmetric(matrix.F2),
      is.symmetric(matrix.T)
    )
  ),
  row.names = FALSE
)
#'
#' ## testthat
#'
#+ testthat_01, echo=TRUE
test_that("is.symmetric is TRUE", {
  expect_true(
    is.symmetric(matrix.T)
  )
})
#'
#+ testthat_02, echo=TRUE
test_that("is.symmetric is FALSE", {
  expect_false(
    is.symmetric(matrix.F1),
    is.symmetric(matrix.F2)
  )
})
#'
#+ testthat_03, echo=TRUE
test_that("is.symmetric is FALSE (expect_error)", {
  expect_error(
    is.symmetric(matrix.F2, stop = TRUE)
  )
})
