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
#' 
#' @import rstanarm
#' @export

bikes_regression <- function(log_scale = TRUE, agc = list(1, 60, TRUE)) {
  
  bikes_d <- bikes_d_log <- yr <- sandy1 <- sandy2 <- NULL

  start_t <- agc[[1]]
  window_length <- agc[[2]]
  df <- gen_atomic_df()
  T <- nrow(oscbvar::bikes_d)

  for (i in (window_length):(T - start_t)) {
    
    mess <- sprintf(
      "iteration %i of %i",
      i - window_length + 1,
      T-start_t + 1 - window_length
    )
    print(mess)

    if (agc[[3]]) {
      j <- i + 1 - window_length
    } else {
      j <- 1
    }

    if (log_scale) {
      data <- oscbvar::bikes_d_log
    } else {
      data <- oscbvar::bikes_d
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
        logcnt ~ . -t,
        data = data[j:i, ],
        cores = 4,
        refresh = 0
      )  
    } else {
      mod <- rstanarm::stan_glm(
        cnt ~ . -t,
        data = data[j:i, ],
        cores = 4,
        refresh = 0
      )
    }
    
    ppd <- rstanarm::posterior_predict(mod, data[i + 1, ])
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