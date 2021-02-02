#' Generates the predictive density for a BVAR
#' 
#' Generates the one-step ahead predictive distribution for a BVAR model with Jeffreys' prior. Returns a list that can be used to get predicitive denistites usinng bvar_osa and bvar_osa_marg.
#' 
#' @param Z Matrix of stacked covariates, with each row corresponding to one observation. Typically lagged values of Y.
#' @param z A vector of covariates for the observation we wish to predict.
#' @param Y A matrix of stacked dependent variables, with each row corresponding to one observation (a vector of (y_1t, ... , ymt)).
#' @param p Number of lags.

bvar_pd <-
function(Z, z, Y, p) {
  T <- nrow(Z)
  m <- ncol(Y)
  d <- ncol(Z)
  k <- m * p + d

  ZtZ <- t(Z) %*% Z
  gamma_hat <- solve(ZtZ, (t(Z) %*% Y))
  S <- t(Y - Z %*% gamma_hat) %*% (Y - Z %*% gamma_hat) 

  M <- t(z) %*% gamma_hat
  P <- 1 + t(z) %*% solve(ZtZ, z)
  Q <-  S
  v <- T - k
  return(list(M, P, Q, v))
}