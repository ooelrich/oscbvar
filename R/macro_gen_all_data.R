#' @title Generate all the data needed for the macroeconomics example
#' 
#' @description Generates all the data needed to create the figures and table
#'   in the macroeconomics example.
#' 
#' @param start_t Which observation in the macrodata to consider as t = 1.
#' @param start_agg At which time point to start aggregating. This will depend
#'   on how much training data you want to feed the algorithms.
#' @param rolling Whether or not to use rolling estimation windows. Defaults to
#'   FALSE.
#' @param window_length Minimum number of observations used for estimation.
#'   Defaults to 60.
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
#' @param tol What tolerance to use in the caliper method.
#' @param woc What weight-on-caliper method to use in the caliper method.

gen_all_data <- function(start_t = 5, start_agg = 161, rolling = FALSE, 
                         window_length = 60, tol = 5, woc = "full",
                         bvar_3 = TRUE, bvar_7 = TRUE,
                         bvar_3_o2 = TRUE, bvar_7_o2 = TRUE,
                         svbvar_3 = TRUE, svbvar_7 = TRUE,
                         svbvar_3_o2 = TRUE, svbvar_7_o2 = TRUE,
                         bart_7 = TRUE, tvpsvbvar_3 = TRUE,
                         bvar_3_basic = FALSE, bvar_7_basic = FALSE) {
    
    df <- gen_atomic_df()

    df_atom <- gen_atomic_preds(window_length = window_length,
                    rolling = rolling, start_t = start_t,
                    bvar_3 = bvar_3, bvar_7 = bvar_7,
                    bvar_3_o2 = bvar_3_o2, bvar_7_o2 = bvar_7_o2,
                    svbvar_3 = svbvar_3, svbvar_7 = svbvar_7,
                    svbvar_3_o2 = svbvar_3_o2, svbvar_7_o2 = svbvar_7_o2,
                    bart_7 = bart_7, tvpsvbvar_3 = tvpsvbvar_3,
                    bvar_3_basic = bvar_3_basic, bvar_7_basic = bvar_7_basic)
    
    agg_preds <- gen_agg_preds(atomic_df = df_atom, start_agg = start_agg,
                                start_t = start_t, baseline = TRUE,
                                caliper = TRUE, mahala = TRUE, tol = tol,
                                woc = "full")
    df <- rbind(df_atom[t >= start_agg, ], agg_preds)
    return(df)
}