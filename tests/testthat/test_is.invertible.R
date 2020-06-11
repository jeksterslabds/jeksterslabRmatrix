#' ---
#' title: "Test: is.invertible"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: is.invertible}
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
context("Test is.invertible.")
#'
#' ## Parameters
#'
#+ parameters
T <- diag(2)
F <- matrix(
  data = 1,
  nrow = 2,
  ncol = 2
)
knitr::kable(
  x = T,
  row.names = FALSE,
  caption = "Invertible Matrix"
)
knitr::kable(
  x = F,
  row.names = FALSE,
  caption = "Non-Invertible Matrix"
)
#'
#' ## Results
#'
#+ results
knitr::kable(
  x = data.frame(
    Item = c(
      "Invertible Matrix",
      "Non-Invertible Matrix"
    ),
    Parameter = c(
      "TRUE",
      "FALSE"
    ),
    Results = c(
      is.invertible(T),
      is.invertible(F)
    )
  ),
  row.names = FALSE
)
#'
#' ## testthat
#'
#+ testthat_01, echo=TRUE
test_that("is.invertible is TRUE", {
  expect_true(
    is.invertible(T)
  )
})
#'
#+ testthat_02, echo=TRUE
test_that("is.invertible is FALSE", {
  expect_false(
    is.invertible(F)
  )
})
#'
#+ testthat_03, echo=TRUE
test_that("is.invertible is FALSE (expect_error)", {
  expect_error(
    is.invertible(F, stop = TRUE)
  )
})
