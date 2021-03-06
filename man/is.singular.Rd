% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is.R
\name{is.singular}
\alias{is.singular}
\title{Singular Matrix}
\usage{
is.singular(X, tol = 1e-08, stop = FALSE)
}
\arguments{
\item{X}{Numeric matrix.
Square matrix.}

\item{tol}{Numeric.
Tolerance.}

\item{stop}{Logical.
If \code{TRUE},
stops and returns an error if \code{X} is not a square matrix.}
}
\value{
Returns \code{TRUE}
if the matrix is not invertible, that is, singular.
Returns \code{FALSE}
if the matrix is invertible.
}
\description{
Checks if a square matrix is non invertible or singular.
}
\references{
\href{https://en.wikipedia.org/wiki/Invertible_matrix}{Wikipedia: Invertible Matrix}
\href{https://en.wikipedia.org/wiki/Singular_matrix}{Wikipedia: Singular Matrix}
}
\author{
Ivan Jacob Agaloos Pesigan
}
