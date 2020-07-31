#' ---
#' title: "Test: is.positive.definite"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: is.positive.definite}
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
context("Test is.positive.definite.")
#'
#' ## Parameters
#'
#+ parameters
matrix.T <- diag(2)
matrix.F <- matrix(
  data = c(1, 2, 2, 1),
  ncol = 2
)
colnames(matrix.T) <- paste0("col", 1:ncol(matrix.T))
rownames(matrix.T) <- paste0("row", 1:nrow(matrix.T))
colnames(matrix.F) <- paste0("col", 1:ncol(matrix.F))
rownames(matrix.F) <- paste0("row", 1:nrow(matrix.F))
knitr::kable(
  x = matrix.F,
  row.names = TRUE,
  caption = "Non-Positive Definite Matrix"
)
knitr::kable(
  x = matrix.T,
  row.names = TRUE,
  caption = "Positive Definite Matrix"
)
#'
#' ## Results
#'
#+ results
knitr::kable(
  x = data.frame(
    Item = c(
      "Non-Positive Definite Matrix",
      "Positive Definite Matrix"
    ),
    Parameter = c(
      "FALSE",
      "TRUE"
    ),
    Results = c(
      is.positive.definite(matrix.F),
      is.positive.definite(matrix.T)
    )
  ),
  row.names = FALSE
)
#'
#' ## testthat
#'
#+ testthat_01, echo=TRUE
test_that("is.positive.definite is TRUE", {
  expect_true(
    is.positive.definite(matrix.T)
  )
})
#'
#+ testthat_02, echo=TRUE
test_that("is.positive.definite is FALSE", {
  expect_false(
    is.positive.definite(matrix.F)
  )
})
#'
#+ testthat_03, echo=TRUE
test_that("is.positive.definite is FALSE (expect_error)", {
  expect_error(
    is.positive.definite(matrix.F, stop = TRUE)
  )
})
