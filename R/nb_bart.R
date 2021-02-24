
#' Generates a notebook for a BART model
#'
#' Uses default settings in dbarts.
#'
#' @param data Dataset from which to generate the notebook
#' @param model Which columns in the dataset should be included
#' @param window_length Minimum length of the estimation window.
#' @param rolling Whether to use a rolling estimation window or not.
#' @param nrep Number of MCMC draws (after burn-in)
#' @param nburn Number of burn-in draws

nb_bart <- function(data, model, window_length = 60, rolling = FALSE,
                       nrep = 10000, nburn = 5000) {

  df <- data.frame(matrix(ncol = 2, nrow = 0))
  x <- c("pred_mean", "dens")
  colnames(df) <- x


  for (i in (window_length + 2):218) {

    if (rolling == TRUE) {
      j <- i - window_length - 1
    } else {
      j <- 1
    }

    Y <- data[(j + 1):(i - 1), model]
    Z <- data[j:(i - 2), model]
    z <- data[i - 1, model]
    y <- data[i, 1]

    bart_model <- dbarts::bart(Z, Y[, 1], z, ndpost = nrep, nskip = nburn)
    kernel_density <- density(bart_model$yhat.test)
    log_pred_dens_bart <- log(approx(kernel_density$x, kernel_density$y, xout = y)$y)

    df[(i - window_length - 1), "pred_mean"][[1]] <- bart_model$yhat.test.mean
    df[(i - window_length - 1), "dens"] <- log_pred_dens_bart

  }
  return(df)
}