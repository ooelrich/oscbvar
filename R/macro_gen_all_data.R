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
#' @param model_list List of names of the atomic models to use.
#' @param tol Parameter that determines caliper tolerance.
#' @param woc Parameter that determines "nonexistence" problems for the caliper.


gen_all_data <- function(model_list, start_t = 5, start_agg = 161, rolling = FALSE, 
                         window_length = 60, tol = 5, woc = "full") {
    
    df <- gen_atomic_df()

    df_atom <- gen_atomic_preds(model_list = model_list, window_length = window_length,
                    rolling = rolling, start_t = start_t)
    
    agg_preds <- gen_agg_preds(atomic_df = df_atom, start_agg = start_agg,
                                start_t = start_t, baseline = TRUE,
                                caliper = TRUE, mahala = TRUE, tol = tol,
                                woc = woc)
    df_atom <- data.table::data.table(df_atom)
    df <- rbind(df_atom[t >= start_agg, ], agg_preds)
    return(df)
}