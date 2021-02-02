#' Predictive density for BVAR models
#' 
#' Multivariate predictive density for BVAR models. Takes an object from bvar_pd as input.
#' 
#' @param y The observed value for which we wish to calculate the predictive density.
#' @param pd A list obtained from bvar_pd with the predictive distribution.
#' @param logscale Whether to return the density on log scale or not. Defaults to TRUE.

bvar_osa <- function(y, pd, logscale = TRUE) {
  M <- pd[[1]]
  P <- pd[[2]]
  Q <- pd[[3]]
  v <- pd[[4]]

  p <- nrow(P)
  q <- nrow(Q)

  part <- rep(NA, q)
  for (i in seq_len(q)) {
    part[i] <- gamma((v + 1 - i) / 2) / gamma((v + p + 1 - i) / 2)
  }

  if (logscale == FALSE) {
    prod_part <- prod(part)
    k <- pi^(p * q / 2) * det(P)^(-q/2) * det(Q)^(-v/2) * prod_part
    expr <- Q + t(y - M) %*% P %*% (y - M)
    dens <- (1/k) * det(expr)^(-(v+p)/2)
  } else {
      logdet_P <- determinant(P, logarithm = TRUE)$modulus[1]
      logdet_Q <- determinant(Q, logarithm = TRUE)$modulus[1]
      log_k <- 0.5 * (p * q * log(pi) - q * logdet_P - v * logdet_Q) +
               sum(log(part)) 
      logdet_big <- determinant(Q + t(y - M) %*% P %*% (y - M))$modulus[1]
      dens <- - log_k - .5 * (v + p) * logdet_big
  }

  return(dens)
}