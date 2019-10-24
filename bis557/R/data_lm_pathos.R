#' lm_patho.csv
#'
#' A dataset containing variables x1, x2, and y, to test that the ridge_regression function works.
#' This tests for an extreme case where two of the explanatory variables are collinear.
#' (i.e. what happens when the data is numerically unstable)
#'
#' @format A data frame with 3 rows and 3 variables:
#' \describe{
#'   \item{y}{response variable}
#'   \item{x1}{explanatory variable 1}
#'   \item{x2}{explanatory variable 2}
#'
#' }
#' @source \url{https://github.com/BIS557/homework-1-lialee912/blob/master/lm_patho.csv}
"lm_patho"
