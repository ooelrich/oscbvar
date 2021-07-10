#' @title Notebook generator: Stochastic BVAR
#' 
#' @description 
#' Generates a notebook for BVAR models with stochastic volatility
#' specifically for the bike sharing data.
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

bikes_svbvar <- function(
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