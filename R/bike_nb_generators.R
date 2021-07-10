#' @title Notebook generator: Stochastic BVAR
#' 
#' @description 
#' Generates a notebook for BVAR models with stochastic volatility
#' specifically for the bike sharing data.
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

  for (i in (window_length):(T - start_t)) {
        
    if (agc[[3]]) {
      j <- i + 1 - window_length
    } else {
      j <- 1
    }

    sv_draws <- stochvol::svsample(
      Y_all[j:i, 1],
      designmatrix = covariates[j:i, ]
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