#'  Generates an empty data frame to store atomic predictions
#' 
#' @keywords internal
#' @NoRd
gen_atomic_df <- function() {
    df <- data.frame(matrix(ncol = 4, nrow = 0))
    x <- c("pmean", "lpdens", "method", "t")
    colnames(df) <- x
    return(df)
}

#' Generates a notebook for a BVAR model v2
#' 
#' Uses Jeffreys' prior. Generates a notebook for the decision-maker to use. 
#' Uses a rolling window of default length 60 to estimate the model.
#' 
#' @param data Dataset from which to generate the notebook
#' @param model Which columns in the dataset should be included
#' @param window_length Minimum length of the estimation window.
#' @param rolling Whether to use a rolling estimation window or not.

nb_bvar <- function(data, model, window_length = 60, rolling = FALSE) {

    df <- gen_atomic_df()
    n <- nrow(data)

    for (i in (window_length + 2):n) {
        
        if (rolling == TRUE) {
            j <- i - window_length - 1
        } else {
            j <- 1
        }

        Y <- data[(j + 1):(i - 1), model]
        Z <- data[j:(i - 2), model]
        z <- data[i - 1, model]
        y <- data[i, 1]
        pdist <- bvar_pd(Z, z, Y, 1)
        pdens <- bvar_osa_marg(y, pdist, 1, TRUE)

        df[(i - window_length - 1), "pmean"][[1]] <- pdist[[1]][1,1]
        df[(i - window_length - 1), "lpdens"] <- pdens
        df[(i - window_length - 1), "method"] <- sprintf("BVAR_%i", length(model))
        df[(i - window_length - 1), "t"] <- i
    }
    return(df)
}


#' Generates a notebook for a simple stochastic volatility model
#' 
#' Uses Gregors prior. Generates a notebook for the decision-maker to use. 
#' Uses a rolling window of default length 60 to estimate the model.
#' 
#' @param data Dataset from which to generate the notebook
#' @param model Which columns in the dataset should be included as covariates.
#' @param window_length Length of estimation window. Defaults to 60.
#' @param rolling Whether to use a rolling estimation window or not.
#' 
#' @import stochvol

nb_stochvol <- function(data, model, window_length = 60, rolling = FALSE) {
    
    df <- gen_atomic_df()
    n <- nrow(data)

    for (i in (window_length + 2):n) {
        
        if (rolling == TRUE) {
            j <- i - window_length - 1
        } else {
            j <- 1
        }

        Y <- data[(j + 1):(i - 1), 1]
        Z <- data[j:(i - 2), model]
        z <- data[i - 1, model]
        y <- data[i, 1]

        sv_draws <- stochvol::svsample(Y,
                           designmatrix = Z)
        pred_draws <- predict(sv_draws, 1, t(z))

        pred_mean <- mean(pred_draws$y[[1]])
        pred_sd <- sd(pred_draws$y[[1]])
        pdens <- dnorm(y, pred_mean, pred_sd, log = TRUE)

        df[(i - window_length - 1), "pmean"] <- pred_mean
        df[(i - window_length - 1), "lpdens"] <- pdens
        df[(i - window_length - 1), "method"] <- sprintf("SVBVAR_%i", length(model))
        df[(i - window_length - 1), "t"] <- i

    }
    return(df)
}



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

  df <- gen_atomic_df()
  n <- nrow(data)

  for (i in (window_length + 2):n) {

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

    df[(i - window_length - 1), "pmean"][[1]] <- bart_model$yhat.test.mean
    df[(i - window_length - 1), "lpdens"] <- log_pred_dens_bart
    df[(i - window_length - 1), "method"] <- sprintf("BART_%i", length(model))
    df[(i - window_length - 1), "t"] <- i

  }
  return(df)
}


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

  df <- gen_atomic_df()
  n <- nrow(data)

  for (i in (window_length + 2):n) {

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
    df[(i - window_length - 1), "method"] <- sprintf("TVPSVBVAR_%i", length(model))
        df[(i - window_length - 1), "t"] <- i

  }
  return(df)
}