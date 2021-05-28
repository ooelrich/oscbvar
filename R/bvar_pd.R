#' @title BVAR-NiW predictive density
#' 
#' @description Generates the predictive density for a BVAR model with 
#'   NiW prior.
#' 
#' @details 
#' Generates the one-step ahead predictive distribution for a BVAR
#' model with normal-Wishart prior (or the flat-Jeff prior). Returns
#' the predictive density and mean corresponding to the variable in
#' `marg`. Density is given on the log scale by default, and the
#' 
#' function currenlty does not support non-logscale. 
#' @param z A vector of covariates for the observation we wish to
#'   predict.
#' @param y A vector of outcome variables, the solution manual if you
#'   will.
#' @param gamma_n Posterior mean for the regression coefficients
#' @param omega_n Posterior covariance-matrix for the regression
#'   coefficients.
#' @param S_n Part of the posterior of the covariance matrix
#' @param nu_n Part of the posterior of the covariance matrix
#' @param marg Which outcome variable we are interested in, defaults to
#'   DGP.
#' @param logscale Whether or not to use logscale. Currently only
#'   logscale is available, so defaults to true and trying to change
#'   this causes and error.
#' 
#' @return A list consisting of the predictive mean and log density of
#'   the variable corresponding to `marg`.

bvar_pd <- function(
    z,
    y,
    gamma_n,
    omega_n,
    S_n,
    nu_n,
    marg = 1,
    logscale = TRUE
  ) {
  
  if (logscale != TRUE) {
    stop("Only supports logscale atm")
  }

  # The full predictive distribution
  M <- t(z) %*% gamma_n
  P <- solve(1 + t(z) %*% omega_n %*% z)
  Q <- S_n
  v <- nu_n
  y <- y[marg]
  
  # marginal predictive density
  dens <- log(gamma((v + 1) / 2) / gamma(v / 2)) -
          0.5 * (log(pi) - log(P) - v * log(Q[marg, marg])) -
          ((v + 1) / 2) * log(Q[marg, marg] + P * (y - M[marg])^2)
  
  return(list(M[marg], dens))
}