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
T <- matrix(
  data = c(
    225, 112.50, 56.25,
    112.5, 225, 112.5,
    56.25, 112.50, 225
  ),
  ncol = 3
)
F1 <- matrix(
  data = 1:10,
  ncol = 2
)
F2 <- matrix(
  data = c(
    225, 0, 0,
    112.5, 225, 0,
    56.25, 112.50, 225
  ),
  ncol = 3
)
knitr::kable(
  x = F1,
  row.names = FALSE,
  caption = "Non-Square Matrix"
)
knitr::kable(
  x = F2,
  row.names = FALSE,
  caption = "Square but not Symmetric Matrix"
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
      "Square but not Symmetric Matrix",
      "Square Matrix"
    ),
    Parameter = c(
      "FALSE",
      "FALSE",
      "TRUE"
    ),
    Results = c(
      is.symmetric(F1),
      is.symmetric(F2),
      is.symmetric(T)
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
    is.symmetric(T)
  )
})
#'
#+ testthat_02, echo=TRUE
test_that("is.symmetric is FALSE", {
  expect_false(
    is.symmetric(F1),
    is.symmetric(F2)
  )
})
#'
#+ testthat_03, echo=TRUE
test_that("is.symmetric is FALSE (expect_error)", {
  expect_error(
    is.symmetric(F2, stop = TRUE)
  )
})