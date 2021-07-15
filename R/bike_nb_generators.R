#' @title Notebook generator: Stochastic BVAR
#' 
#' @description 
#' Generates a notebook for BVAR models with stochastic volatility
#' specifically for the bike sharing data. For some reason the svsample
#' function does not work when any of the columns in covariates have 
#' "to many zeroes" (more than half?), in which case you need to
#' specify "startpara = list(mu = 0, phi = 0, sigma = 1, beta = rep(0,
#' ncol(covariates))."
#' 
#' @details
#' Uses the stochvol package with default priors. 
#' 
#' @param covariates Covariates to use in the model, in additional to
#'   the count variable (which is always included). No defaults.
#' @param log_scale Boolean. Do you want to take the log of the count
#'   variable? Defaults to FALSE.
#' @param agc List of atomic prediction generation controllers. The 
#'   first element of the list gives the starting time (ie what  
#'   observation is considered as t = 1), the second element is the 
#'   minimum window length used for estimation, and the third one is a 
#'   boolean indicating if the estimation window is rolling or not.
#' @param include_intercept Should the design matrix include an
#'   intercept? Defaults to TRUE.
#' 
#' @import stochvol

bikes_svbvar <- function(
    covariates,
    log_scale = FALSE,
    agc = list(1, 60, FALSE),
    include_intercept = TRUE
) {

  covariates <- as.matrix(covariates)

  if (log_scale) {
      data <- bikes_d$logcnt
  } else {
      data <- bikes_d$cnt
  }

  df <- gen_atomic_df()
  T <- length(data)
  start_t <- agc[[1]]
  window_length <- agc[[2]]

  Y_all <- as.matrix(data[start_t:T])

  inter <- rep(1, nrow(covariates))
  covariates <- cbind(inter, covariates)

  for (i in (window_length):(T - start_t)) {
        
    if (agc[[3]]) {
      j <- i + 1 - window_length
    } else {
      j <- 1
    }

    sv_draws <- stochvol::svsample(
      Y_all[j:i, 1],
      designmatrix = covariates[j:i, ],
      startpara = list(
        mu = 0,
        phi = 0.1,
        sigma = 1,
        beta = rep(0, ncol(covariates))
      )
    )
    pred_draws <- predict(sv_draws, 1, t(covariates[i + 1, ]))

    pred_mean <- mean(pred_draws$y[[1]])
    pred_sd <- sd(pred_draws$y[[1]])
    pdens <- dnorm(Y_all[i + 1, 1], pred_mean, pred_sd, log = TRUE)

    df[(i + 1 - window_length), "pmean"] <- pred_mean
    df[(i + 1 - window_length), "lpdens"] <- pdens
    df[(i + 1 - window_length), "method"] <- sprintf("SVBVAR")
    df[(i + 1 - window_length), "t"] <- (i + 1)
    df[(i + 1 - window_length), "ytrue"] <- Y_all[i + 1, 1]
  }
  return(df)
}



#' @title Notebook generator: BART
#'
#' @description
#' Generates a notebook for a BART model using the bike sharing data.
#' 
#' @details 
#' Uses default settings in dbarts.
#'
#' @param covariates Covarites, aka design matrix.
#' @param log_scale Should the dependent variable be on log scale?
#' @param agc List of atomic prediction generation controllers. The 
#'   first element of the list gives the starting time (ie what  
#'   observation is considered as t = 1), the second element is the 
#'   minimum window length used for estimation, and the third one is a 
#'   boolean indicating if the estimation window is rolling or not.
#' @param include_intercept Whether to include an intercep in the
#'   design matrix or not. Defaults to FALSE since the intercept breaks
#'   the function.
#' @param nrep Number of MCMC draws (after burn-in)
#' @param nburn Number of burn-in draws
#' 
#' @import sn

bikes_bart <-function(
    covariates,
    log_scale = FALSE,
    agc = list(1, 60, TRUE),
    include_intercept = FALSE,
    nrep = 10000,
    nburn = 5000
) {

  stopifnot(is.logical(agc[[3]]))
  stopifnot(is.logical(include_intercept))

  if (log_scale) {
      data <- matrix(bikes_d$logcnt, ncol = 1)
  } else {
      data <- matrix(bikes_d$cnt, ncol = 1)
  }

  start_t <- agc[[1]]
  window_length <- agc[[2]]
  df <- gen_atomic_df()
  T <- nrow(data)
  Y_all <-as.matrix(data.frame(data[start_t:T, 1]))
  
  Z_all <- covariates

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
    
    Y <- matrix(Y_all[j:i, 1])
    Z <- Z_all[j:i, ]
    z <- Z_all[i + 1, ]
    y <- Y_all[i + 1, 1]
    xall <- cbind(matrix(Y_all), Z_all)

    sigmat <- cbind(Y, Z) # For some reason Sebastians formula no work
    Sigma.OLS <- sigma(lm(Y ~ ., sigmat))^2
    prior.sig <- c(NROW(Y)/2, 0.75)
    
    bart_model <- dbarts::dbarts(
      V1 ~ .,
      data = sigmat,
      control = control,
      tree.prior = dbarts:::cgm(cgm.exp, cgm.level), 
      node.prior = dbarts:::normal(sd.mu),
      n.samples = nrep, 
      weights = rep(1, NROW(Y)), 
      sigma = sqrt(Sigma.OLS[1]), 
      resid.prior = dbarts:::chisq(prior.sig[[1]], prior.sig[[2]])
    )
    est_mod <- bart_model$run()
    preds <- bart_model$predict(z, NULL)[1,]

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
    df[(i + 1 - window_length), "method"] <- "BART"
    df[(i + 1 - window_length), "t"] <- (i + 1)
    df[(i + 1 - window_length), "ytrue"] <- Y_all[i + 1, 1]
    
  }
  return(df)
}



#' @title Notebook generator: simple regression
#'
#' @description
#' Generates a notebook for a bayesian regression model.
#' 
#' @details 
#' rstanarm default yes.
#'
#' @param log_scale Should the dependent variable be on log scale?
#' @param agc List of atomic prediction generation controllers. The 
#'   first element of the list gives the starting time (ie what  
#'   observation is considered as t = 1), the second element is the 
#'   minimum window length used for estimation, and the third one is a 
#'   boolean indicating if the estimation window is rolling or not.
#' @param include_intercept Whether to include an intercep in the
#'   design matrix or not. Defaults to FALSE since the intercept breaks
#'   the function.
#' @param nrep Number of MCMC draws (after burn-in)
#' @param nburn Number of burn-in draws
#' 
#' @import sn

bikes_regression <- function(log_scale = FALSE, agc = list(1, 60, TRUE)) {
  
  start_t <- agc[[1]]
  window_length <- agc[[2]]
  df <- gen_atomic_df()
  T <- nrow(bikes_d)

  for (i in (window_length):(T - start_t)) {
    
    mess <- sprintf("iteration %i of %i", i - window_length + 1, T-start_t)
    print(mess)
    if (agc[[3]]) {
      j <- i + 1 - window_length
    } else {
      j <- 1
    }

    if (log_scale) {
      data <- bikes_d_log
    } else {
      data <- bikes_d
    }
    
    if (i < 365) {
      data <- subset(data, select = -c(yr, sandy1, sandy2))
    } else if (i < 667) {
      data <- subset(data, select = -c(sandy1, sandy2))
    } else if (i == 667) {
      data <- subset(data, select = -c(sandy2))
    }

    if (log_scale) {
      mod <- rstanarm::stan_glm(
        logcnt ~ .,
        data = data[j:i, ],
        cores = 4,
        refresh = 0
      )  
    } else {
      mod <- rstanarm::stan_glm(
        cnt ~ .,
        data = data[j:i, ],
        cores = 4,
        refresh = 0
      )
    }
    
    ppd <- rstanarm::posterior_predict(mod, data)
    pred_mean <- mean(ppd)
    pred_sd <- sd(ppd)
    lpdens <- dnorm(
      data[i + 1, 1][[1]],
      mean = pred_mean,
      sd = pred_sd,
      log = TRUE
    )
  
    methodd <- ifelse(log_scale, "BREGLOG", "BREG")
    df[(i + 1 - window_length), "pmean"] <- pred_mean
    df[(i + 1 - window_length), "lpdens"] <- lpdens
    df[(i + 1 - window_length), "method"] <- methodd
    df[(i + 1 - window_length), "t"] <- (i + 1)
    df[(i + 1 - window_length), "ytrue"] <- data[i + 1, 1]    
  }
  return(df)
}