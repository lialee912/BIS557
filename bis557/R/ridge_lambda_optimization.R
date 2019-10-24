#' Optimizing Ridge Regression Parameter Lambda
#'
#' @description This is a method to optimize selection of the parameter lambda in ridge regression. Note:
#' a cross validation approach will be adopted to select the best value for lambda, which involves splitting
#' the data into a training and test set. We want a model to be fit to the training set with a specific value
#' of lambda, then determine values of beta coeffiecients and compare to those derived from the test data set.
#' We will repeat this process for many different values of lambda and choose the model with the highest
#' accuracy on the test set.
#'
#' @param form Formula of y~x1+x2+...
#' @param data The data set from which one desires to build the linear model
#' @param folds Folds refers to the number of groups that a given data sample should be split into
#' @param lambdas Lambda parameter to use for ridge regression
#' @return One value providing the optimal lambda parameter to use for ridge regression
#' @importFrom magrittr %>%
#' @importFrom doParallel registerDoParallel
#' @importFrom rsample vfold_cv testing training
#' @importFrom foreach %do% %dopar%
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#' @import stats
#' @import casl
#' @import crayon
#' @import foreach
#' @export

ridge_lambda_optimization <- function(form, data, folds = 10, lambdas=seq(0,0.5,0.01)) {

  #initialization
  i <- NULL
  lambdas <- NULL
  '.' <- NULL

  #From class notes, initialize functions ridge_regression and predict.ridge_regression
  ridge_regression <- function(form, data, lambda = 0) {
    rownames(data) <- NULL
    X <- model.matrix(form, data)
    Y <- data[[as.character(form)[2]]][as.numeric(rownames(X))]
    ret <- solve( crossprod(X) + diag(rep(lambda, ncol(X))) ) %*% t(X) %*% Y
    attributes(ret)$formula <- form
    class(ret) <- c(class(ret), "ridge_regression")
    ret
  }

  predict.ridge_regression <- function(object, ...) {
    dots <- list(...)
    x_frame <- dots[[1]]
    if (!is.data.frame(x_frame)) {
      stop(red("The first argument should be a data.frame of values",
               "to predict"))
    }
    X <- model.matrix(attributes(object)$formula, x_frame)
    X %*% object
  }

  #Get the number of folds
  folds <- vfold_cv(data[sample.int(nrow(data), 50),], folds)

  # Assign the training and test data set
  train1 <- training(folds$splits[[1]])
  test1 <- testing(folds$splits[[1]])

  registerDoParallel(6) #break up these processes into 6

  #Calculate RMSE: root mean squared error for each possible lambda
  rmses <- foreach(lambda = lambdas, .combine = rbind) %dopar% {
    foreach(i = seq_len(nrow(folds)), .combine = c) %do% {

      casl_util_rmse(testing(folds$splits[[i]])$duration,
                     predict(ridge_regression(form, training(folds$splits[[i]]),
                                              lambda = lambda),
                             testing(folds$splits[[i]])))
    }
  }

  #This will create a tibble that provides values RMSE, SD, lambda, and the upper and lower values
  edf <- tibble(mean = apply(rmses, 1, mean),
                sd = apply(rmses, 1, sd),
                lambda = lambdas) %>%
    mutate(upper = mean + 2 * sd / sqrt(nrow(.)),
           lower = mean - 2 * sd / sqrt(nrow(.)))

  #Now, we want to find the row which produces the minimum RMSE and identify the lambda value associated with it
  row=which(edf[,1]== min(edf[,1]))
  lambda=edf[row,3] #column 3 corresponds to the lambda value

  return(lambda)
}
