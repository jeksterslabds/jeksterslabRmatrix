#' ---
#' title: "Test: is.square"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: is.square}
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
context("Test is.square.")
#'
#' ## Parameters
#'
#+ parameters
T <- matrix(
  data = 1:9,
  nrow = 3
)
F <- matrix(
  data = 1:10,
  ncol = 2
)
knitr::kable(
  x = F,
  row.names = FALSE,
  caption = "Non-Square Matrix"
)
knitr::kable(
  x = T,
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
      "Square Matrix"
    ),
    Parameter = c(
      "FALSE",
      "TRUE"
    ),
    Results = c(
      is.square(F),
      is.square(T)
    )
  ),
  row.names = FALSE
)
#'
#' ## testthat
#'
#+ testthat_01, echo=TRUE
test_that("is.square is TRUE", {
  expect_true(
    is.square(T)
  )
})
#'
#+ testthat_02, echo=TRUE
test_that("is.square is FALSE", {
  expect_false(
    is.square(F)
  )
})
#'
#+ testthat_03, echo=TRUE
test_that("is.square is FALSE (expect_error)", {
  expect_error(
    is.square(F, stop = TRUE)
  )
})
