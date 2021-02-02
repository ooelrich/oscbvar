#' Marginal predictive density for BVAR models
#' 
#' Marginal predictive density for BVAR models. Takes an object from bvar_pd as input.
#' 
#' @param y The observed value for which we wish to calculate the predictive density.
#' @param pd A list obtained from bvar_pd with the predictive distribution.
#' @param marg Which variable to get the marginal for.
#' @param logscale Whether to return the density on log scale or not. Defaults to TRUE.

bvar_osa_marg <- function(y, pd, marg, logscale = TRUE) {
  M <- pd[[1]][marg]
  P <- pd[[2]]
  Q <- pd[[3]][marg, marg]
  v <- pd[[4]]
  y <- y[marg]

  if (logscale == FALSE) {
    stop("please use logscale")
  } else {
    dens <- log(gamma((v + 1) / 2) / gamma(v / 2)) -
            0.5 * (log(pi) - log(P) - v * log(Q)) -
            ((v + 1) / 2) * log(Q + P * (y - M)^2)
  }
  return(dens)
}