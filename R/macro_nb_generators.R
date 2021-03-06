
    
#' @title Notebook generator: BVAR NiW
#' 
#' @description
#' Generates a notebook for a BVAR model with a Minnesota-flavoured NiW prior.
#' 
#' @details
#' Generates a notebook of predictions for the decision maker to use. Uses a 
#' version of the Minnesota prior: the prior for the variance is data based
#' with nu_0 set to the number of time series plus two, and S_0 obtained by
#' running a simple AR(4) model and extracting the diagonal elements. The prior
#' for the regression coefficients is tweaked using two hyperparameters: the
#' overall tightness (defaults to 0.2) and the lag decay rate (deftaults to 1).
#' The cross-variable tightness is set to 1 to retain the Kronecker structure
#' required for conjugacy.
#' 
#' @param data Dataset from which to generate the notebook. Should include only
#'   the variables used by the model, and the outcome variable of interest
#'   should be in column 1.
#' @param window_length Minimum length of the estimation window. Defaults to
#'   60 quarters (i.e. 5 years).
#' @param rolling Whether to use a rolling estimation window or not. Defaults to
#'   FALSE.
#' @param start_t Which observation starts the estimation sample. Defaults to 5.
#' @param lags The order of the VAR model. Defaults to 1. 
#' @param overall_tightness Overall tightness (pi_1 in Sunes notation). Defaults
#'   to 0.2
#' @param lag_decay The lag decay rate. Defaults to 1.
#' @param include_intercept Whether or not to include an intercept. Defaults to
#'   TRUE.

nb_bvar <- function(data, window_length = 60, rolling = FALSE,
                    start_t = 5, lags = 1, overall_tightness = 0.2,
                    lag_decay = 1, include_intercept = TRUE) {

    # data frame to store predictions in, all notebooks use the same function
    df <- gen_atomic_df() 

    T <- nrow(data)
    m <- ncol(data)
    Y_all <- as.matrix(data.frame(data[start_t:T, ]))
    Z_all <- gen_Z(data = data.frame(data), start_t = start_t, lags = lags,
                   include_intercept = include_intercept)

    # Determine the priors that are constant over time
    nu_0 <- m + 2
    omega_0 <- omega_minnesota(lags, overall_tightness, lag_decay, m, include_intercept)

    # Start generating predictions
    for (i in window_length:(T - start_t)) {
        
        if (rolling == TRUE) { # j is the starting point of the estimation set
            j <- i + 1 - window_length
        } else {
            j <- 1
        }

        Y <- Y_all[j:i, ]
        Z <- Z_all[j:i, ]
        z <- Z_all[i + 1, ]
        y <- Y_all[i + 1, ]
        ZtZ <- t(Z) %*% Z
        ZtY <- t(Z) %*% Y
        beta_ols <- solve(ZtZ, ZtY)
        S <- t(Y - Z %*% beta_ols) %*% (Y - Z %*% beta_ols)

        # Determine the S_0 part of the iW-prior
        S_0 <- diag(diag(S))

        # Calculate the posterior
        #omega_n <- solve(solve(omega_0) + ZtZ)
        #gamma_n <- omega_n %*% t(Z) %*% Y
        #nu_n <- nrow(Y) + nu_0
        #S_n <- S_0 + S + t(beta_ols) %*% solve(omega_0 + solve(ZtZ), beta_ols)

        # set to flat-Jeff for testing (REMOVE ME LATER)
        omega_n <- solve(ZtZ)
        gamma_n <- beta_ols
        S_n <- S
        nu_n <- nrow(Z) - ncol(Z)

        pdist <- bvar_pd(z, y, gamma_n = gamma_n, omega_n = omega_n, S_n = S_n,
                        nu_n = nu_n, marg = 1, logscale = TRUE)

        df[(i + 1 - window_length), "pmean"][[1]] <- pdist[[1]]
        df[(i + 1 - window_length), "lpdens"] <- pdist[[2]]
        df[(i + 1 - window_length), "method"] <- sprintf("BVAR_%i_o%i", m, lags)
        df[(i + 1 - window_length), "t"] <- i
    }
    return(df)
}


#' @title Notebook generator: Stochastic BVAR
#' 
#' @description 
#' Generates a notebook for BVAR models with stochastic volatility.
#' 
#' @details
#' Uses the stochvol package with default priors. 
#' 
#' @param data Dataset from which to generate the notebook. Should include only
#'   the variables used by the model, and the outcome varaible of interest
#'   should be in column 1.
#' @param window_length Length of estimation window. Defaults to 60 quarters 
#'   (i.e. 5 years).
#' @param rolling Whether to use a rolling estimation window or not. Defaults to
#'   FALSE.
#' @param start_t Which observation to consider as t=1.
#' @param lags The order of the VAR model. Defaults to 1.
#' @param include_intercept Should the design matrix include an intercept? 
#'   Defaults to TRUE.
#' 
#' @import stochvol

nb_svbvar <- function(data, window_length = 60, rolling = FALSE,
                    start_t = 5, lags = 1, include_intercept = TRUE) {
    
    df <- gen_atomic_df()
    T <- nrow(data)
    m <- ncol(data)

    Y_all <- as.matrix(data.frame(data[start_t:T, ]))
    Z_all <- gen_Z(data.frame(data), start_t, lags, include_intercept)

    for (i in (window_length):(T - start_t)) {
        
        if (rolling == TRUE) {
            j <- i + 1 - window_length
        } else {
            j <- 1
        }

        sv_draws <- stochvol::svsample(Y_all[j:i, 1],
                           designmatrix = Z_all[j:i, ])
        pred_draws <- predict(sv_draws, 1, t(Z_all[i + 1, ]))

        pred_mean <- mean(pred_draws$y[[1]])
        pred_sd <- sd(pred_draws$y[[1]])
        pdens <- dnorm(Y_all[i + 1, 1], pred_mean, pred_sd, log = TRUE)

        df[(i + 1 - window_length), "pmean"] <- pred_mean
        df[(i + 1 - window_length), "lpdens"] <- pdens
        df[(i + 1 - window_length), "method"] <- sprintf("SVBVAR_%i", m)
        df[(i + 1 - window_length), "t"] <- i

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
#' @param start_t Which observation to set as t=1.
#' @param nrep Number of MCMC draws (after burn-in)
#' @param nburn Number of burn-in draws

nb_bart <- function(data, model, window_length = 60, rolling = FALSE,
                    start_t = 5, nrep = 10000, nburn = 5000) {

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
#' @param start_t Which observation to consider as t=1.
#' @param nrep Number of MCMC draws (after burn-in)
#' @param nburn Number of burn-in draws
#' @param tau Number of observations to use for training prior.

nb_tvpsvbvar <- function(data, model, window_length = 60, rolling = FALSE,
                       start_t = 5, nrep = 10000, nburn = 5000, tau = 20) {

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

    df[(i - window_length - 1), "pmean"][[1]] <- mean(bv$fc.mdraws[1, 1, ])
    df[(i - window_length - 1), "lpdens"] <- log_pred_dens_bvar
    df[(i - window_length - 1), "method"] <- sprintf("TVPSVBVAR_%i", length(model))
    df[(i - window_length - 1), "t"] <- i

  }
  return(df)
}


#' Generates a notebook for a BVAR simple version
#' 
#' Uses a flat Jeffreys' prior. Generates a notebook for the decision-maker to use. No intercept. One lag. Very simple. 
#' 
#' @param data Dataset from which to generate the notebook
#' @param window_length Minimum length of the estimation window.
#' @param rolling Whether to use a rolling estimation window or not.

nb_bvar_flat_Jeff <- function(data, window_length = 60, rolling = FALSE) {

    df <- gen_atomic_df()
    n <- nrow(data)
    m <- ncol(data)
    data <- data.frame(data)

    for (i in (window_length + 2):n) {
        
        if (rolling == TRUE) {
            j <- i - window_length - 1
        } else {
            j <- 1
        }

        Y <- data[(j + 1):(i - 1), ]
        Z <- data[j:(i - 2), ]
        z <- data[i - 1, ]
        y <- data[i, 1]
        ZtZ <- t(Z) %*% Z
        ZtY <- t(Z) %*% Y
        beta_ols <- solve(ZtZ, ZtY)

        gamma_n <- beta_ols
        omega_n <- solve(ZtZ)
        S_n <- t(Y - Z %*% beta_ols) %*% (Y - Z %*% beta_ols)
        nu_n <- nrow(Y) - ncol(Y)

        pdist <- bvar_pd(z, y, gamma_n, omega_n, S_n, nu_n)

        df[(i - window_length - 1), "pmean"][[1]] <- pdist[[1]]
        df[(i - window_length - 1), "lpdens"] <- pdist[[2]]
        df[(i - window_length - 1), "method"] <- sprintf("BVAR_%i", m)
        df[(i - window_length - 1), "t"] <- i
    }
    return(df)
}