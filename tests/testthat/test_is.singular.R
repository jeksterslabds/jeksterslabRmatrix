#' ---
#' title: "Test: is.singular"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: is.singular}
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
context("Test is.singular.")
#'
#' ## Parameters
#'
#+ parameters
matrix.T <- matrix(
  data = 1,
  nrow = 2,
  ncol = 2
)
matrix.F <- diag(2)
colnames(matrix.T) <- paste0("col", 1:ncol(matrix.T))
rownames(matrix.T) <- paste0("row", 1:nrow(matrix.T))
colnames(matrix.F) <- paste0("col", 1:ncol(matrix.F))
rownames(matrix.F) <- paste0("row", 1:nrow(matrix.F))
knitr::kable(
  x = matrix.F,
  row.names = TRUE,
  caption = "Non-Singular Matrix"
)
knitr::kable(
  x = matrix.T,
  row.names = TRUE,
  caption = "Singular Matrix"
)
#'
#' ## Results
#'
#+ results
knitr::kable(
  x = data.frame(
    Item = c(
      "Non-Singular Matrix",
      "Singular Matrix"
    ),
    Parameter = c(
      "FALSE",
      "TRUE"
    ),
    Results = c(
      is.singular(matrix.F),
      is.singular(matrix.T)
    )
  ),
  row.names = FALSE
)
#'
#' ## testthat
#'
#+ testthat_01, echo=TRUE
test_that("is.singular is TRUE", {
  expect_true(
    is.singular(matrix.T)
  )
})
#'
#+ testthat_02, echo=TRUE
test_that("is.singular is FALSE", {
  expect_false(
    is.singular(matrix.F)
  )
})
