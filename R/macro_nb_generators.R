#' @title Notebook generator: BVAR NiW
#' 
#' @description
#' Generates a notebook for a BVAR model with a Minnesota-flavoured NiW 
#' prior.
#' 
#' @details
#' Generates a notebook of predictions for the decision maker to use. 
#' Uses a version of the Minnesota prior: the prior for the variance is 
#' data based with nu_0 set to the number of time series plus two, and 
#' S_0 obtained by running a simple AR(4) model and extracting the 
#' diagonal elements. The prior for the regression coefficients is 
#' tweaked using two hyperparameters: the overall tightness (defaults
#' to 0.2) and the lag decay rate (deftaults to 1).
#' The cross-variable tightness is set to 1 to retain the Kronecker 
#' structure required for conjugacy.
#' 
#' @param data Dataset from which to generate the notebook. Should
#'   include only the variables used by the model. Defaults to the
#'   macroeconomic dataset from this package using all varaibles.
#' @param agc List of atomic prediction generation controllers. The 
#'   first element of the list gives the starting time (ie what  
#'   observation is considered as t = 1), the second element is the 
#'   minimum window length used for estimation, and the third one is a 
#'   boolean indicating if the estimation window is rolling or not, the 
#'   forth indicates which variable is the response variable and it 
#'   defaults to 1 (GDP).
#' @param lags The order of the VAR model. Defaults to 1. 
#' @param overall_tightness Overall tightness (pi_1 in Sunes notation).
#'   Defaults to 0.2
#' @param lag_decay The lag decay rate. Defaults to 1.
#' @param include_intercept Whether or not to include an intercept. 
#'   Defaults to TRUE.

nb_bvar <- function(
    data = oscbvar::macrodata[, 1:7],
    agc = list(5, 60, TRUE, 1),
    lags = 1,
    overall_tightness = 0.2,
    lag_decay = 1,
    include_intercept = TRUE
  ) {

  df <- gen_atomic_df() # data frame to store predictions in

  start_t <- agc[[1]]
  window_length <- agc[[2]]
  T <- nrow(data)
  m <- ncol(data)
  Y_all <- as.matrix(data.frame(data[start_t:T, ]))
  Z_all <- gen_Z(data = data.frame(data), start_t, lags, include_intercept)
  response <- agc[[4]]

  # Set the part of the prior that is constant over time
  nu_0 <- m + 2
  omega_0 <- omega_minnesota(
    lags,
    overall_tightness,
    lag_decay,
    m,
    include_intercept
  )

  # Start generating predictions
  for (i in window_length:(T - start_t)) {
        
    if (agc[[3]]) { # j is the starting point of the estimation set
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
    omega_n <- solve(solve(omega_0) + ZtZ)
    gamma_n <- omega_n %*% t(Z) %*% Y
    nu_n <- nrow(Y) + nu_0
    S_n <- S_0 + S + t(beta_ols) %*% solve(omega_0 + solve(ZtZ), beta_ols)

    pdist <- bvar_pd(
      z,
      y,
      gamma_n = gamma_n,
      omega_n = omega_n,
      S_n = S_n,
      nu_n = nu_n,
      marg = response,
      logscale = TRUE)

    df[(i + 1 - window_length), "pmean"] <- pdist[[1]]
    df[(i + 1 - window_length), "lpdens"] <- pdist[[2]]
    df[(i + 1 - window_length), "method"] <- sprintf("BVAR_%i_o%i", m, lags)
    df[(i + 1 - window_length), "t"] <- (i + 1)
    df[(i + 1 - window_length), "ytrue"] <- y[response]
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
#' @param data Dataset from which to generate the notebook. Should 
#'   include only the variables used by the model. Defaults to the
#'   macroeconomic data set from this package using all variables.
#' @param agc List of atomic prediction generation controllers. The 
#'   first element of the list gives the starting time (ie what  
#'   observation is considered as t = 1), the second element is the 
#'   minimum window length used for estimation, and the third one is a 
#'   boolean indicating if the estimation window is rolling or not, the 
#'   forth indicates which variable is the response variable and it 
#'   defaults to 1 (GDP).
#' @param lags The order of the VAR model. Defaults to 1.
#' @param include_intercept Should the design matrix include an
#'   intercept? Defaults to TRUE.
#' 
#' @import stochvol

nb_svbvar <- function(
    data = oscbvar::macrodata[, 1:7],
    agc = list(5, 60, TRUE, 1),
    lags = 1,
    include_intercept = TRUE
) {

  df <- gen_atomic_df()
  T <- nrow(data)
  m <- ncol(data)
  start_t <- agc[[1]]
  window_length <- agc[[2]]
  response <- agc[[4]]

  Y_all <- as.matrix(data.frame(data[start_t:T, ]))
  Z_all <- gen_Z(
    data.frame(data),
    start_t,
    lags,
    include_intercept
  )

  for (i in (window_length):(T - start_t)) {
        
    if (agc[[3]]) {
      j <- i + 1 - window_length
    } else {
      j <- 1
    }

    sv_draws <- stochvol::svsample(
      Y_all[j:i, response],
      designmatrix = Z_all[j:i, ]
    )
    pred_draws <- predict(sv_draws, 1, t(Z_all[i + 1, ]))

    pred_mean <- mean(pred_draws$y[[1]])
    pred_sd <- sd(pred_draws$y[[1]])
    pdens <- dnorm(Y_all[i + 1, response], pred_mean, pred_sd, log = TRUE)

    df[(i + 1 - window_length), "pmean"] <- pred_mean
    df[(i + 1 - window_length), "lpdens"] <- pdens
    df[(i + 1 - window_length), "method"] <- sprintf("SVBVAR_%i_o%i", m, lags)
    df[(i + 1 - window_length), "t"] <- (i + 1)
    df[(i + 1 - window_length), "ytrue"] <- Y_all[i + 1, response]
  }
  return(df)
}


#' @title Notebook generator: BART
#'
#' @description
#' Generates a notebook for a BART model.
#' 
#' @details 
#' Uses default settings in dbarts.
#'
#' @param data Dataset from which to generate the notebook. Defaults to
#'   the macroeconomic data set from this package using all variables.
#' @param agc List of atomic prediction generation controllers. The 
#'   first element of the list gives the starting time (ie what  
#'   observation is considered as t = 1), the second element is the 
#'   minimum window length used for estimation, and the third one is a 
#'   boolean indicating if the estimation window is rolling or not, the 
#'   forth indicates which variable is the response variable and it 
#'   defaults to 1 (GDP).
#' @param lags The order of the VAR. Defaults to 1.
#' @param include_intercept Whether to include an intercep in the
#'   design matrix or not. Defaults to FALSE since the intercept breaks
#'   the function.
#' @param nrep Number of MCMC draws (after burn-in)
#' @param nburn Number of burn-in draws
#' 
#' @import sn

nb_bart <-function(
    data = oscbvar::macrodata[, 1:7],
    agc = list(5, 60, TRUE, 1),
    lags = 1,
    include_intercept = FALSE,
    nrep = 10000,
    nburn = 5000
) {

  stopifnot(is.logical(agc[[3]]))
  stopifnot(is.logical(include_intercept))

  start_t <- agc[[1]]
  window_length <- agc[[2]]
  response <- agc[[4]]
  df <- gen_atomic_df()
  T <- nrow(data)
  m <- ncol(data)
  Y_all <-as.matrix(data.frame(data[start_t:T, ]))
  
  Z_all <- gen_Z(
    data.frame(data),
    start_t,
    lags,
    include_intercept
  )

  cgm.level <- 0.95 # alpha
  cgm.exp <- 2 # beta
  num.trees <- 250 # S
  prior.cov <- 0.01
  sd.mu <- 2
  
  control <- dbarts::dbartsControl(
    verbose = FALSE,
    keepTrainingFits = TRUE, 
    useQuantiles = FALSE,
    keepTrees = TRUE,
    n.samples = nrep,
    n.cuts = 100L,
    n.burn = nburn,
    n.trees = num.trees, 
    n.chains = 1,
    n.threads = 1,
    n.thin = 1L,
    printEvery = 1,
    printCutoffs = 0L,
    rngKind = "default", 
    rngNormalKind = "default",
    updateState = FALSE
  )
  
  for (i in (window_length):(T - start_t)) {
    
    if (agc[[3]]) {
      j <- i + 1 - window_length
    } else {
      j <- 1
    }
    
    Y <- Y_all[j:i, ]
    Z <- Z_all[j:i, ]
    z <- Z_all[i + 1, ]
    y <- Y_all[i + 1, response]
    
    Sigma.OLS <- sigma(lm(Y~Z))^2
    prior.sig <- c(NROW(Y)/2, 0.75)
    
    bart_model <- dbarts::dbarts(
      Y[, response]~Z, 
      control = control,
      tree.prior = cgm(cgm.exp, cgm.level), 
      node.prior = normal(sd.mu),
      n.samples = nrep, 
      weights = rep(1, NROW(Y)), 
      sigma = sqrt(Sigma.OLS[1]), 
      resid.prior = chisq(prior.sig[[1]], prior.sig[[2]])
    )
    est_mod <- bart_model$run()
    preds <- bart_model$predict(z, NULL)[1, ]
    
    kernel_density <- density(preds)
    
    log_score_st <- function(post_pred_draws, y_new) {
      obj <- sn::selm(post_pred_draws ~ 1, family = "st")
      para <- as.list(coef(obj, param.type = "DP"))
      sn::dst(
        y_new,
        xi = para$xi,
        omega = para$omega,
        alpha = para$alpha,
        nu = para$nu,
        log = TRUE
      )
    }

    log_pred_dens_bart <- log_score_st(preds, y)

    df[(i + 1 - window_length), "pmean"] <- mean(preds)
    df[(i + 1 - window_length), "lpdens"] <- log_pred_dens_bart
    df[(i + 1 - window_length), "method"] <- sprintf("BART_%i_o%i", m, lags)
    df[(i + 1 - window_length), "t"] <- (i + 1)
    df[(i + 1 - window_length), "ytrue"] <- Y_all[i + 1, response]
    
  }
  return(df)
}

#' @title Notebook generator: TVP-SV-BVAR
#'
#' @description
#' Generates a notebook for a BVAR model with stochastic variance and 
#' time-varying parameters.
#' 
#' @details  
#' Uses default settings in bvarsv.
#'
#' @param data Dataset from which to generate the notebook. Defaults to
#'   the macrodata set from the package including only the first three
#'   variables.
#' @param agc List of atomic prediction generation controllers. The 
#'   first element of the list gives the starting time (ie what  
#'   observation is considered as t = 1), the second element is the 
#'   minimum window length used for estimation, and the third one is a 
#'   boolean indicating if the estimation window is rolling or not, the 
#'   forth indicates which variable is the response variable and it 
#'   defaults to 1 (GDP).
#' @param nrep Number of MCMC draws (after burn-in)
#' @param nburn Number of burn-in draws
#' @param tau Number of observations to use for training prior.
#' 
#' @import bvarsv

nb_tvpsvbvar <- function(
    data = oscbvar::macrodata[, 1:3],
    agc = list(5, 60, TRUE, 1),
    nrep = 10000,
    nburn = 5000,
    tau = 20
) {

  stopifnot(is.logical(agc[[3]]))

  response <- agc[[4]]
  df <- gen_atomic_df()
  T <- nrow(data)
  m <- ncol(data)
  start_t <- agc[[1]]
  window_length <- agc[[2]]

  Y_all <- as.matrix(data[start_t:T, ])

  for (i in window_length:(T - start_t)) {

    if (agc[[3]]) {
      j <- i + 1 - window_length
    } else {
      j <- 1
    }

    Y <- Y_all[j:i, ]
    y <- Y_all[i + 1, response]

    bv <- bvarsv::bvar.sv.tvp(
      Y,
      nf = 1, # Number of future time periods for which to forecast
      nrep = nrep,
      nburn = nburn,
      tau = tau
    )
    # in predict, v is the variable of interest, and h the forecast horizon
    f <- bvarsv::predictive.density(bv, v = response, h = 1)
    log_pred_dens_bvar <- log(f(y))

    df[(i + 1 - window_length), "pmean"] <- mean(bv$fc.mdraws[response, 1, ])
    df[(i + 1 - window_length), "lpdens"] <- log_pred_dens_bvar
    df[(i + 1 - window_length), "method"] <- sprintf("TVPSVBVAR_%i", m)
    df[(i + 1 - window_length), "t"] <- i + 1
    df[(i + 1 - window_length), "ytrue"] <- Y_all[i + 1, response]

  }
  return(df)
}