#' Ridge Regression
#'
#' @description This is an implementation of ridge regression that takes into account colinear
#' (or nearly colinear) regression variables.
#'
#' @param form Formula of y~x1+x2+...
#' @param data The data set from which one desires to build the linear model
#' @param contrasts List of contrasts for factor (categorical) variables
#' @param lambda Ridge regression parameter
#' @return List of beta coefficients derived from a linear regression
#' @import stats
#' @export
#'
ridge_regression <- function(form,data, lambda=0.01, contrasts = NULL) {
  model_matrix1 <- model.matrix(form,data,contrasts)

  #Establish X and Y
  X <- model_matrix1
  Y <- matrix(data[,as.character(form)[2]], ncol=1)

  mean_x = colMeans(X[,-1])
  mean_y = mean(Y)
  n = nrow(X)

  #Rescaling the variables
  X <- X[,-1] - rep(mean_x, rep(n, ncol(X)-1)) #rescale X
  ncol_X = ncol(X)
  Y <- Y - mean_y #rescale Y
  scale <- drop(sqrt(rep(1/n, n) %*% (X^2)))
  X <- X/rep(scale, rep(n, ncol_X))

  beta <- matrix(NA_real_, nrow=length(1),ncol=ncol_X) #initialize beta
  svd_X <- svd(X)
  beta <- svd_X$v %*% diag(svd_X$d/(svd_X$d^2 + lambda)) %*% t(svd_X$u) %*% Y #solve for beta
  beta <- t(as.matrix(beta/scale))
  beta_0 <- mean_y - beta%*% mean_x #solve for intercept

  beta_ridge <- as.vector(cbind(beta_0, beta)) #combine all values together for a list of all coefficients
  names(beta_ridge) <- c("Intercept", colnames(X))

  #Return list of coefficients
  return(list(coef=beta_ridge))
}
