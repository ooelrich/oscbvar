#' @title Generate atomic predictions
#' 
#' @description 
#' Generates a notebook of atomic predictive densities and means.
#' 
#' @details 
#' Function to generate the atomic predictions (that is the predictions of the
#' individual models, not of aggregation schemes) for the macro-data example 
#' for predicting DGP. Essentialy a list of potential models to include that the
#' user selects from. Also contains some global settings that always should be
#' the same between models, such as estimation window length, starting time, and
#' which data set to use. Uses the data set previously known as og_medium_scaled
#' so to minimize the risk of accidentally using the wrong data set...
#' 
#' @param model_list List of names of the atomic models to use.
#' @param agc List of atomic prediction generation controllers. The first
#'   element of the list gives the starting time (ie what observation is 
#'   considered as t = 1), the second element is the minimum window length used
#'   for estimation, and the third one is a boolean indicating if the estimation
#'   window is rolling or not.

gen_atomic_preds <- function(model_list, agc = list(5, 60, FALSE, 1)) {

    df <- gen_atomic_df()

    if ("bvar_3" %in% model_list) {
        dat <- nb_bvar(oscbvar::macrodata[, 1:3], agc, lags = 1)
        df <- rbind(df, dat)
    }
    if ("bvar_7" %in% model_list) {
        dat <- nb_bvar(oscbvar::macrodata[, 1:7], agc, lags = 1)
        df <- rbind(df, dat)
    }
    if ("bvar_3_o2" %in% model_list) {
        dat <- nb_bvar(oscbvar::macrodata[, 1:3], agc, lags = 2)
        df <- rbind(df, dat)
    }
    if ("bvar_7_o2" %in% model_list) {
        dat <- nb_bvar(oscbvar::macrodata[, 1:7], agc, lags = 2)
        df <- rbind(df, dat)
    }
    if ("svbvar_3" %in% model_list) {
        dat <- nb_svbvar(oscbvar::macrodata[, 1:3], agc, lags = 1,
                        include_intercept = TRUE)
        df <- rbind(df, dat)
    }
    if ("svbvar_7" %in% model_list) {
        dat <- nb_svbvar(oscbvar::macrodata[, 1:7], agc, lags = 1,
                        include_intercept = TRUE)
        df <- rbind(df, dat)
    }
    if ("svbvar_3_o2" %in% model_list) {
        dat <- nb_svbvar(oscbvar::macrodata[, 1:3], agc, lags = 2,
                        include_intercept = TRUE)
        df <- rbind(df, dat)
    }
    if ("svbvar_7_o2" %in% model_list) {
        dat <- nb_svbvar(oscbvar::macrodata[, 1:7], agc, lags = 2,
                        include_intercept = TRUE)
        df <- rbind(df, dat)
    }
    if ("bart_7" %in% model_list) {
        dat <- nb_bart(oscbvar::macrodata[, 1:7], agc)
        df <- rbind(df, dat)
    }
    if ("tvpsvbvar_3" %in% model_list) {
        dat <- nb_tvpsvbvar(oscbvar::macrodata[, 1:3], agc)
        df <- rbind(df, dat)
    }

    return(df)
}
