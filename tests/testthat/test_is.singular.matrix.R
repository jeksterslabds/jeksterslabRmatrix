#' ---
#' title: "Test: is.singular.matrix"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: is.singular.matrix}
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
context("Test is.singular.matrix.")
#'
#' ## Parameters
#'
#+ parameters
F <- diag(2)
T <- matrix(
  data = 1,
  nrow = 2,
  ncol = 2
)
knitr::kable(
  x = F,
  row.names = FALSE,
  caption = "Non-Singular Matrix"
)
knitr::kable(
  x = T,
  row.names = FALSE,
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
      is.singular.matrix(F),
      is.singular.matrix(T)
    )
  ),
  row.names = FALSE
)
#'
#' ## testthat
#'
#+ testthat_01, echo=TRUE
test_that("is.singular.matrix is TRUE", {
  expect_true(
    is.singular.matrix(T)
  )
})
#'
#+ testthat_02, echo=TRUE
test_that("is.singular.matrix is FALSE", {
  expect_false(
    is.singular.matrix(F)
  )
})
