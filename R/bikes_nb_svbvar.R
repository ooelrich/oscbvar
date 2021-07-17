#' @title Notebook generator: Stochastic BVAR
#' 
#' @description 
#' Generates a notebook for BVAR models with stochastic volatility
#' specifically for the bike sharing data. For some reason the svsample
#' function does not work when any of the columns in covariates have 
#' "to many zeroes" (more than half?), in which case you need to
#' specify "startpara = list(mu = 0, phi = 0, sigma = 1, beta = rep(0,
#' ncol(covariates)).".
#' 
#' @details
#' Uses the stochvol package with default priors. 
#' 
#' @param log_scale Boolean. Do you want to take the log of the count
#'   variable? Defaults to TRUE.
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
    log_scale = TRUE,
    agc = list(1, 60, FALSE),
    include_intercept = TRUE
) {

  logcnt <- NULL # NSE R CMD check NOTE

  if(!log_scale){
    stop("Non-log version not yet implemented")
  }

  if (log_scale) {
      data <- oscbvar::bikes_d_log
  } else {
      data <- oscbvar::bikes_d
  }

  covariates <- subset(data, select = -c(logcnt, t))
  covariates <- as.matrix(covariates)

  df <- gen_atomic_df()
  T <- nrow(data)
  start_t <- agc[[1]]
  window_length <- agc[[2]]

  Y_all <- as.matrix(data[start_t:T, "logcnt"])

  inter <- rep(1, nrow(covariates))
  covariates <- cbind(inter, covariates)

  for (i in (window_length):(T - start_t)) {
        
    print(sprintf("%i of %i", i-window_length+1, T-start_t+1-window_length))

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
