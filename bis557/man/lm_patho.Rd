% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_lm_pathos.R
\docType{data}
\name{lm_patho}
\alias{lm_patho}
\title{lm_patho.csv}
\format{A data frame with 3 rows and 3 variables:
\describe{
  \item{y}{response variable}
  \item{x1}{explanatory variable 1}
  \item{x2}{explanatory variable 2}

}}
\source{
\url{https://github.com/BIS557/homework-1-lialee912/blob/master/lm_patho.csv}
}
\usage{
lm_patho
}
\description{
A dataset containing variables x1, x2, and y, to test that the ridge_regression function works.
This tests for an extreme case where two of the explanatory variables are collinear.
(i.e. what happens when the data is numerically unstable)
}
\keyword{datasets}
