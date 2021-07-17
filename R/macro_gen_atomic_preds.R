#' @title Generate atomic predictions
#' 
#' @description 
#' Generates a notebook of atomic predictive densities and means.
#' 
#' @details 
#' Function to generate the atomic predictions (that is the predictions
#' of the individual models, not of aggregation schemes) for the
#' macrodata example. Essentialy a list of potential models to include
#' that the user selects from. Also contains some global settings that
#' always should be the same between models, such as estimation window
#' length, starting time, and which data set to use.
#' 
#' @param model_list List of names of the atomic models to use.
#' @param agc List of atomic prediction generation controllers with
#'   four elements:
#'   \enumerate{
#'     \item Which observation to consider as the first observation
#'           (ie t=1). Defaults to 5, which gives some breathing room
#'           for the specification of AR models.
#'     \item Size of the estimation window. This determines how many
#'           observations are used to train the models before any
#'           predictions are made. Defaults to 60.
#'     \item Whether to use a rolling estimation window or not. The 
#'           default is FALSE, which corresponds to a non-rolling
#'           window.
#'     \item Which variable should be the response variable. Defaults
#'           to 3 (FEDFUNDS). Other possible values are 1 (GDP) and 2
#'           (GDPTCPI).
#'   }
#' 
#' @return A data frame of atomic predictions with columns
#'   \enumerate{
#'     \item \code{pmean}, the predictive mean for time t (made at t-1)
#'     \item \code{lpdens}, the log predictive density for time t 
#'           (made at t - 1)
#'     \item \code{method}, which model made the prediction
#'     \item \code{t}, timepoint
#'     \item \code{ytrue}, true value of the response at time t
#'   }

gen_atomic_preds <- function(model_list, agc = list(5, 60, FALSE, 1)) {

    df <- gen_atomic_df()

    if ("bvar_3" %in% model_list) {
        dat <- nb_bvar(macrodata[, 1:3], agc, lags = 1)
        df <- rbind(df, dat)
    }
    if ("bvar_7" %in% model_list) {
        dat <- nb_bvar(macrodata[, 1:7], agc, lags = 1)
        df <- rbind(df, dat)
    }
    if ("bvar_3_o2" %in% model_list) {
        dat <- nb_bvar(macrodata[, 1:3], agc, lags = 2)
        df <- rbind(df, dat)
    }
    if ("bvar_7_o2" %in% model_list) {
        dat <- nb_bvar(macrodata[, 1:7], agc, lags = 2)
        df <- rbind(df, dat)
    }
    if ("svbvar_3" %in% model_list) {
        dat <- nb_svbvar(macrodata[, 1:3], agc, lags = 1,
                        include_intercept = TRUE)
        df <- rbind(df, dat)
    }
    if ("svbvar_7" %in% model_list) {
        dat <- nb_svbvar(macrodata[, 1:7], agc, lags = 1,
                        include_intercept = TRUE)
        df <- rbind(df, dat)
    }
    if ("svbvar_3_o2" %in% model_list) {
        dat <- nb_svbvar(macrodata[, 1:3], agc, lags = 2,
                        include_intercept = TRUE)
        df <- rbind(df, dat)
    }
    if ("svbvar_7_o2" %in% model_list) {
        dat <- nb_svbvar(macrodata[, 1:7], agc, lags = 2,
                        include_intercept = TRUE)
        df <- rbind(df, dat)
    }
    if ("bart_7" %in% model_list) {
        dat <- nb_bart(macrodata[, 1:7], agc)
        df <- rbind(df, dat)
    }
    if ("tvpsvbvar_3" %in% model_list) {
        dat <- nb_tvpsvbvar(macrodata[, 1:3], agc)
        df <- rbind(df, dat)
    }

    return(df)
}
