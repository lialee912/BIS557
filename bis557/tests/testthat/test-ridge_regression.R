library(MASS)
library(testthat)

context("Test the output of homework 2 regarding ridge regression.")

test_that("Your ridge_regression() function works in an easy case.", {

  data(iris)

  fit_ridge_regression <- ridge_regression(Sepal.Length ~ ., iris, lambda = 0.01)

  fit_lm.ridge <- lm.ridge(Sepal.Length  ~ ., iris, lambda = 0.01)

  expect_equivalent(coef(fit_lm.ridge), fit_ridge_regression$coef,
                    tolerance = 1e-5)
})

test_that("Your ridge_regression() function works with contrasts.", {

  data(iris)

  fit_ridge_regression <- ridge_regression(Sepal.Length ~ ., iris, lambda = 0.01,
                                           contrasts = list(Species = "contr.sum"))

  fit_lm.ridge <- lm.ridge(Sepal.Length  ~ ., iris, lambda = 0.01, contrasts = list(Species = "contr.sum"))

  expect_equivalent(coef(fit_lm.ridge), fit_ridge_regression$coef,
                    tolerance = 1e-5)
})

test_that("Your ridge_regression() function works in a tougher case.", {

  data(lm_patho)

  fit_ridge_regression <- ridge_regression(y ~., lm_patho, lambda= 0.01)

  fit_lm.ridge <- lm.ridge(y ~., lambda = 0.01, lm_patho)

  expect_equivalent(coef(fit_lm.ridge), fit_ridge_regression$coef,
                    tolerance = 1e-5)
})
