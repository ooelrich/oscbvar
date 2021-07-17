#' @title Notebook generator: BART
#'
#' @description
#' Generates a notebook for a BART model using the bike sharing data.
#' 
#' @details 
#' Uses default settings in dbarts.
#'
#' @param agc List of atomic prediction generation controllers. The 
#'   first element of the list gives the starting time (ie what  
#'   observation is considered as t = 1), the second element is the 
#'   minimum window length used for estimation, and the third one is a 
#'   boolean indicating if the estimation window is rolling or not.
#' @param include_intercept Whether to include an intercep in the
#'   design matrix or not. Defaults to FALSE since the intercept breaks
#'   the function
#' @param nrep Number of MCMC draws (after burn-in)
#' @param nburn Number of burn-in draws
#' 
#' @import sn
#' @importFrom stats dnorm density lm
#' @export

bikes_bart <-function(
    agc = list(1, 200, TRUE),
    include_intercept = FALSE,
    nrep = 10000,
    nburn = 5000
) {

  bikes_d <- cgm <- chisq <- NULL

  data <- matrix(oscbvar::bikes_d_log$logcnt, ncol = 1)

  start_t <- agc[[1]]
  window_length <- agc[[2]]
  df <- gen_atomic_df()
  T <- nrow(data)
  Y_all <-as.matrix(data.frame(data[start_t:T, 1]))
  
  Z_all <- subset(oscbvar::bikes_d_log, select = -c(logcnt, t))

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
    
    print(sprintf("%i of %i", i-window_length+1, T-start_t+1-window_length))

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
      Y ~ .,
      data = sigmat,
      control = control,
      tree.prior = cgm(cgm.exp, cgm.level), 
      node.prior = normal(sd.mu),
      n.samples = nrep, 
      weights = rep(1, NROW(Y)), 
      sigma = sqrt(Sigma.OLS[1]), 
      resid.prior = chisq(prior.sig[[1]], prior.sig[[2]])
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