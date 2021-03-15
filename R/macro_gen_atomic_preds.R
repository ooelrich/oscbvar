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
#' @param window_length Minimum number of observations used for estimation.
#'   Defaults to 60.
#' @param rolling Whether to use a rolling window for estimation, defaults to
#'   false.
#' @param start_t Which observation to consider the first real observation. All
#'   observations prior to that one will be considered fixed. Set to 5 by
#'   default to allow the specification of models that are autoregressive of
#'   order 4 without breaking anything.
#' @param model_list List of names of the atomic models to use.

gen_atomic_preds <- function(model_list, window_length = 60,
                             rolling = FALSE, start_t = 5) {

    df <- gen_atomic_df()

    if ("bvar_3" %in% model_list) {
        dat <- nb_bvar(oscbvar::macrodata[, 1:3], window_length = window_length, 
                        rolling = rolling, start_t = start_t, lags = 1)
        df <- rbind(df, dat)
    }
    if ("bvar_7" %in% model_list) {
        dat <- nb_bvar(oscbvar::macrodata[, 1:7], window_length = window_length,
                        rolling = rolling, start_t = start_t, lags = 1)
        df <- rbind(df, dat)
    }
    if ("bvar_3_o2" %in% model_list) {
        dat <- nb_bvar(oscbvar::macrodata[, 1:3], window_length = window_length, 
                        rolling = rolling, start_t = start_t, lags = 2)
        df <- rbind(df, dat)
    }
    if ("bvar_7_o2" %in% model_list) {
        dat <- nb_bvar(oscbvar::macrodata[, 1:7], window_length = window_length,
                        rolling = rolling, start_t = start_t, lags = 2)
        df <- rbind(df, dat)
    }
    if ("svbvar_3" %in% model_list) {
        dat <- nb_svbvar(oscbvar::macrodata[, 1:3], window_length = window_length,
                        rolling = rolling, start_t = start_t, lags = 1,
                        include_intercept = TRUE)
        df <- rbind(df, dat)
    }
    if ("svbvar_7" %in% model_list) {
        dat <- nb_svbvar(oscbvar::macrodata[, 1:7], window_length = window_length,
                        rolling = rolling, start_t = start_t, lags = 1,
                        include_intercept = TRUE)
        df <- rbind(df, dat)
    }
    if ("svbvar_3_o2" %in% model_list) {
        dat <- nb_svbvar(oscbvar::macrodata[, 1:3], window_length = window_length,
                        rolling = rolling, start_t = start_t, lags = 2,
                        include_intercept = TRUE)
        df <- rbind(df, dat)
    }
    if ("svbvar_7_o2" %in% model_list) {
        dat <- nb_svbvar(oscbvar::macrodata[, 1:7], window_length = window_length,
                        rolling = rolling, start_t = start_t, lags = 2,
                        include_intercept = TRUE)
        df <- rbind(df, dat)
    }
    if ("bart_7" %in% model_list) {
        dat <- nb_bart(oscbvar::macrodata[, 1:7], window_length = window_length,
                        rolling = rolling, start_t = start_t)
        df <- rbind(df, dat)
    }
    if ("tvpsvbvar_3" %in% model_list) {
        dat <- nb_tvpsvbvar(oscbvar::macrodata[, 1:3], window_length = window_length,
                    rolling = rolling, start_t = start_t)
        df <- rbind(df, dat)
    }

    return(df)
}
