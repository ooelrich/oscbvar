#' @title Notebook generator: simple linear regression
#'
#' @description
#' Generates a notebook for a frequentist regression model.
#' 
#' @details 
#' lm() basically
#'
#' @param log_scale Should the dependent variable be on log scale?
#' @param agc List of atomic prediction generation controllers. The 
#'   first element of the list gives the starting time (ie what  
#'   observation is considered as t = 1), the second element is the 
#'   minimum window length used for estimation, and the third one is a 
#'   boolean indicating if the estimation window is rolling or not.
#' 
#' @export

bikes_linreg <- function(log_scale = TRUE, agc = list(1, 200, TRUE)) {
  
  bikes_d <- bikes_d_log <- yr <- sandy1 <- sandy2 <- NULL

  start_t <- agc[[1]]
  window_length <- agc[[2]]
  df <- gen_atomic_df()
  T <- nrow(oscbvar::bikes_d)

  for (i in (window_length):(T - start_t)) {
    
    print(sprintf("%i of %i", i-window_length+1, T-start_t+1-window_length))

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
      mod <- lm(
        logcnt ~ . -t,
        data = data[j:i, ]
      )  
    } else {
      print("PLZ log")
    }
    
    ppd <- predict(mod, data[i + 1, ])
  
    methodd <- ifelse(log_scale, "LOGLM", "LM")
    df[(i + 1 - window_length), "pmean"] <- ppd[[1]]
    df[(i + 1 - window_length), "lpdens"] <- NA_real_
    df[(i + 1 - window_length), "method"] <- methodd
    df[(i + 1 - window_length), "t"] <- (i + 1)
    df[(i + 1 - window_length), "ytrue"] <- data[i + 1, 1]    
  }
  return(df)
}