#' @title Generate aggregate predictions
#' 
#' @description Super-function that generates aggregate predictions a la carte.
#' 
#' @param atomic_df Data frame containing the atomic predictions to base the 
#'   aggregate predictions on.
#' @param start_agg Which time point to generate the first aggregate prediction
#'   for.
#' @param start_t Which observation is t = 1. Used to get the correct state of
#'   the world from macrodata.
#' @param baseline Whether to generate the baseline aggregations (gewisano and
#'   equal weights). Defaults to TRUE.
#' @param  caliper Whether to generate RAP based on the caliper method. Defaults 
#'   to TRUE.
#' @param mahala Whether to generate RAP based on the mahalanobis method. 
#'   Defaults to TRUE.
#' @param tol Tolerance parameter for the caliper method. Defaults to 5.
#' @param woc Weight on caliper, parameter for the caliper method. Defaults to
#'   "full."
#' 
#' @import data.table

gen_agg_preds <- function(atomic_df, start_agg, start_t = 5, baseline = TRUE,
                          caliper = TRUE, mahala  = TRUE, tol = 5, woc = "full") {

    df_agg <- gen_atomic_df()
    sotw <- macrodata[start_t:nrow(macrodata), ]
    sotw <- data.frame(t = c(1:nrow(sotw)), sotw)

    if (baseline) {
        df_base <- gen_baseline(atomic_df, start_agg)
        df_agg <- rbind(df_agg, df_base)
    }
    if (caliper) {
        weight_df <- caliper_relevance(atomic_df, sotw, start_agg, tol, woc)
        RAL_data <- RAL_calculator(weight_df, atomic_df)
        df_cal_prop <- gen_RAA(RAL_data, "propto")
        df_cal_sel <- gen_RAA(RAL_data, "select_best")
        df_agg <- rbind(df_agg, df_cal_prop, df_cal_sel)
    }
    if (mahala) {
        weight_df <- mahala_relevance(atomic_df, sotw, start_agg)
        RAL_data <- RAL_calculator(weight_df, atomic_df)
        df_mahala_prop <- gen_RAA(RAL_data, "propto")
        df_mahala_sel <- gen_RAA(RAL_data, "select_best")
        df_agg <- rbind(df_agg, df_mahala_prop, df_mahala_sel)
    }
    return(df_agg)
}