## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(linearmodel)

## ------------------------------------------------------------------------
library(casl)
n <- 1000; p <- 25 
beta <- c(1, rep(0, p-1)) #create regression vector beta with 1st coordinate 1 and the rest 0
X <- matrix(rnorm(n*p), ncol=p) #data matrix X with randomly sampled normal variables

svals <- svd(X)$d
max(svals) / min(svals) #calculating the condition # of the matrix X --> (1.36)

#generate y and see how close beta_OLS is to the true beta
n <- 1000
count_errors <- rep(0, n)
for (k in 1:n) {
  y <- X %*% beta + rnorm(n)
  betahat <- casl_ols_svd(X,y)
  count_errors[k] <- sqrt(sum((betahat - beta)^2))
}
mean(count_errors) #typical error is small = 0.158

#replace 1st column of X with linear combo of 1st and 2nd column (--> 2 X columns highly correlated) leading to higher condition # for matrix X^T X

alpha <- 0.001
X[,1] <- X[,1] * alpha + X[,2] * (1-alpha) 
svals <- svd(X)$d
max(svals) / min(svals) #condition number high = 2000

#rerunning simulation, we see that the error rate increased significantly
n <- 1000
count_errors <- rep(0, n)
for (k in 1:n) {
  y <- X %*% beta + rnorm(n)
  betahat <- solve(crossprod(X), crossprod(X,y))
  count_errors[k] <- sqrt(sum((betahat - beta)^2))
}
mean(count_errors)

## ------------------------------------------------------------------------
alpha <- 0.001
lambda <- 0.01
X <- matrix(rnorm(n*p), ncol=p) #redo X
X[,1] <- X[,1] * alpha + X[,2] * (1-alpha) 
svals <- svd(X)$d
max(svals) + lambda / min(svals) +lambda #condition number = 46

n <- 1000
check_errors <- rep(0, n)
for (k in 1:n) {
  y <- X %*% beta + rnorm(n)
  betahat_ridge <- solve( crossprod(X) + diag(rep(lambda, ncol(X))) ) %*% t(X) %*% y #change to beta_hat_ridge
  check_errors[k] <- sqrt(sum((betahat_ridge - beta)^2))
}

mean(check_errors)

