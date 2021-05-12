#' @title Generate all the data needed for the macroeconomics example
#' 
#' @description Generates all the data needed to create the figures and table
#'   in the macroeconomics example.
#' 
#' @param model_list List of names of the atomic models to use.
#' @param agc List of atomic prediction generation controllers. The first
#'   element of the list gives the starting time (ie what observation is 
#'   considered as t = 1), the second element is the minimum window length used
#'   for estimation, and the third one is a boolean indicating if the estimation
#'   window is rolling or not.
#' @param start_agg At which time point to start aggregating. This will depend
#'   on how much training data you want to feed the algorithms.
#' @param tol Parameter that determines caliper tolerance.
#' @param woc Parameter that determines "nonexistence" problems for the caliper.
#' @param gen_atom Should atomic data be generated or is it supplied? Defaults 
#'   to TRUE.
#' @param atom_df Optional argument, supplies a data frame with atomic
#'   predictions.


gen_all_data <- function(
        model_list,
        agc = list(5, 60, FALSE),
        start_agg = 161,
        tol = 5,
        woc = "full",
        gen_atom = TRUE,
        atom_df = NULL
    ) {
    
    df <- gen_atomic_df()
    if (gen_atom) {
        df_atom <- gen_atomic_preds(model_list, agc)
    } else {
        df_atom <- atom_df
    }
    
    agg_preds <- gen_agg_preds(atomic_df = df_atom, start_agg = start_agg,
                                baseline = TRUE,
                                caliper = TRUE, mahala = TRUE, tol = tol,
                                woc = woc)
    df_atom <- data.table::data.table(df_atom)
    df <- rbind(df_atom[t >= start_agg, ], agg_preds)
    return(df)
}