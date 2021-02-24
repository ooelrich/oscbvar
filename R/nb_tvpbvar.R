#' Generates a notebook for a TVP-SV-BVAR model
#'
#' Uses default settings in bvarsv.
#'
#' @param data Dataset from which to generate the notebook
#' @param model Which columns in the dataset should be included
#' @param window_length Minimum length of the estimation window.
#' @param rolling Whether to use a rolling estimation window or not.
#' @param nrep Number of MCMC draws (after burn-in)
#' @param nburn Number of burn-in draws
#' @param tau Number of observations to use for training prior.

nb_tvpbvar <- function(data, model, window_length = 60, rolling = FALSE,
                       nrep = 10000, nburn = 5000, tau = 20) {

  df <- data.frame(matrix(ncol = 2, nrow = 0))
  x <- c("pred_mean", "dens")
  colnames(df) <- x


  for (i in (window_length + 2):218) {

    if (rolling == TRUE) {
      j <- i - window_length - 1
    } else {
      j <- 1
    }

    Y <- data[j:(i - 1), model]
    y <- data[i, 1]

    bv <- bvarsv::bvar.sv.tvp(Y, nf = 1, nrep = nrep, nburn = nburn, tau = tau)
    f <- bvarsv::predictive.density(bv, v = 1, h = 1)
    log_pred_dens_bvar <- log(f(y))

    df[(i - window_length - 1), "pred_mean"][[1]] <- mean(bv$fc.mdraws[1, 1, ])
    df[(i - window_length - 1), "dens"] <- log_pred_dens_bvar

  }
  return(df)
}