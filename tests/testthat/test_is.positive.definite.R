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
F <- matrix(
  data = c(1, 2, 2, 1),
  ncol = 2
)
T <- diag(2)
knitr::kable(
  x = F,
  row.names = FALSE,
  caption = "Non-Positive Definite Matrix"
)
knitr::kable(
  x = T,
  row.names = FALSE,
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
      is.positive.definite(F),
      is.positive.definite(T)
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
    is.positive.definite(T)
  )
})
#'
#+ testthat_02, echo=TRUE
test_that("is.positive.definite is FALSE", {
  expect_false(
    is.positive.definite(F)
  )
})
