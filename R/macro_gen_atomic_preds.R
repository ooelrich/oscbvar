
#' @title Generate atomic predictions
#' 
#' @description 
#' Generates a notebook of atomic predictive densities and means.
#' 
#' @details 
#' Function to generate the atomic predictions (that is the predictions of the
#' individual models, not of aggregation scehemes) for the macro-data example 
#' for predicting DGP. Essentialy a list of potential models to include that the
#' user selects from. Also contains some global settings that always should be
#' the same between models, such as estimation window length, starting time, and
#' which data set to use. Uses the data set previously known as og_medium_scaled
#' so to minimize the risk of accidentally using the wrong data set...
#' 
#' @param window_length How many observations to use for estimation.
#' @param rolling Whether to use a rolling window for estimation, defaults to
#'   false.
#' @param start_t Which observation to consider the first real observation. All
#'   observations prior to that one will be considered fixed. Set to 5 by
#'   default to allow the specification of models that are autoregressive of
#'   order 4 without breaking anything.
#' @param bvar_3 Should the 3-dimensional VAR(1) model be included?
#' @param bvar_7 Should the 7-dimensional VAR(1) model be included?
#' @param bvar_3_o2 Should the 3-dimensional VAR(2) model be included?
#' @param bvar_7_o2 Should the 7-dimensional VAR(2) model be included?
#' @param svbvar_3 Should the 3-dimensional BVAR with stochastic volatility be
#'   included?
#' @param svbvar_7 Should the 7-dimensional BVAR with stochastic volatility be
#'   included?
#' @param svbvar_3_o2 Should the 3-dimensional VAR(2) with stochastic volatility
#'   be included?
#' @param svbvar_7_o2 Should the 7-dimensional VAR(2) with stochastic volatility
#'   be included?
#' @param bart_7 Should the 7-dimensional BART model be included?
#' @param tvpsvbvar_3 Should the 3-dimensional time-varying-parameter BVAR with
#'   stochastic volatility be included?
#' @param bvar_3_basic Should the basic 3-dimensional flat-Jeff VAR(1) model be
#'   included?
#' @param bvar_7_basic Should the basic 7-dimensional flat-Jeff VAR(1) model be
#'   included?

gen_atomic_preds <- function(window_length = 60,
                             rolling = FALSE, start_t = 5,
                             bvar_3 = TRUE, bvar_7 = TRUE,
                             bvar_3_o2 = TRUE, bvar_7_o2 = TRUE,
                             svbvar_3 = TRUE, svbvar_7 = TRUE,
                             svbvar_3_o2 = TRUE, svbvar_7_o2 = TRUE,
                             bart_7 = TRUE, tvpsvbvar_3 = TRUE,
                             bvar_3_basic = FALSE, bvar_7_basic = FALSE) {

    df <- gen_atomic_df()

    if (bvar_3 == TRUE) {
        dat <- nb_bvar(oscbvar::macrodata[, 1:3], window_length = window_length, 
                        rolling = rolling, start_t = start_t, lags = 1)
        df <- rbind(df, dat)
    }
    if (bvar_7 == TRUE) {
        dat <- nb_bvar(oscbvar::macrodata[, 1:7], window_length = window_length,
                        rolling = rolling, start_t = start_t, lags = 1)
        df <- rbind(df, dat)
    }
    if (bvar_3_o2 == TRUE) {
        dat <- nb_bvar(oscbvar::macrodata[, 1:3], window_length = window_length, 
                        rolling = rolling, start_t = start_t, lags = 2)
        df <- rbind(df, dat)
    }
    if (bvar_7_o2 == TRUE) {
        dat <- nb_bvar(oscbvar::macrodata[, 1:7], window_length = window_length,
                        rolling = rolling, start_t = start_t, lags = 2)
        df <- rbind(df, dat)
    }
    if (svbvar_3 == TRUE) {
        dat <- nb_svbvar(oscbvar::macrodata[, 1:3], window_length = window_length,
                        rolling = rolling, start_t = start_t, lags = 1,
                        include_intercept = TRUE)
        df <- rbind(df, dat)
    }
    if (svbvar_7 == TRUE) {
        dat <- nb_svbvar(oscbvar::macrodata[, 1:7], window_length = window_length,
                        rolling = rolling, start_t = start_t, lags = 1,
                        include_intercept = TRUE)
        df <- rbind(df, dat)
    }
    if (svbvar_3_o2 == TRUE) {
        dat <- nb_svbvar(oscbvar::macrodata[, 1:3], window_length = window_length,
                        rolling = rolling, start_t = start_t, lags = 2,
                        include_intercept = TRUE)
        df <- rbind(df, dat)
    }
    if (svbvar_7_o2 == TRUE) {
        dat <- nb_svbvar(oscbvar::macrodata[, 1:7], window_length = window_length,
                        rolling = rolling, start_t = start_t, lags = 2,
                        include_intercept = TRUE)
        df <- rbind(df, dat)
    }
    if (bart_7 == TRUE) {
        dat <- nb_bart(oscbvar::macrodata, c(1:7), window_length = window_length,
                        rolling = rolling, start_t = start_t)
        df <- rbind(df, dat)
    }
    if (tvpsvbvar_3 == TRUE) {
        dat <- nb_tvpsvbvar(oscbvar::macrodata, c(1:3), window_length = window_length,
                    rolling = rolling, start_t = start_t)
        df <- rbind(df, dat)
    }

    return(df)
}
