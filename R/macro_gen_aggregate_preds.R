#' @title Generate aggregate predictions
#' 
#' @description 
#' Super-function that generates aggregate predictions a la carte. Carte.
#' 
#' @param atomic_df Data frame containing the atomic predictions to
#'   base the aggregate predictions on.
#' @param start_agg Which time point to generate the first aggregate
#'   prediction for.
#' @param sotw Decision maker data set, observation one should
#'   correspond to start_t used to generate the atomic predictions.
#' @param baseline Whether to generate the baseline aggregations
#'   (gewisano and equal weights). Defaults to TRUE.
#' @param  caliper Whether to generate RAP based on the caliper method.
#'   Defaults to TRUE.
#' @param mahala Whether to generate RAP based on the mahalanobis
#'   method. Defaults to TRUE.
#' @param cw Tolerance parameter for the caliper method. Defaults to
#'   5. Caliper width.
#' @param mvc Minimum viable cluster size, ie minimum amount of
#'   observations required within the cluster to not combine with the
#'   global mean.
#' 
#' @import data.table

gen_agg_preds <- function(
        atomic_df,
        start_agg,
        sotw,
        baseline = TRUE,
        caliper = TRUE,
        mahala  = TRUE,
        cw = 5,
        mvc = 10
) {

    df_agg <- gen_atomic_df()
    

    if (baseline) {
        df_base <- gen_baseline(atomic_df, start_agg)
        df_agg <- rbind(df_agg, df_base)
    }

    if (caliper) {

        weight_df <- caliper_relevance(
            atomic_df,
            sotw,
            start_agg,
            cw,
            mvc
        )
        
        RAL_data <- RAL_calculator(weight_df, atomic_df)
        df_cal_prop <- gen_RAA(RAL_data, "propto", "caliper")
        # df_cal_sel <- gen_RAA(RAL_data, "select_best", "caliper")
        df_agg <- rbind(
            df_agg, 
            df_cal_prop
            #,df_cal_sel
        )
    }

    if (mahala) {

        #weight_df <- mahala_relevance(atomic_df, sotw, start_agg)
        RAL_data <- RAL_calculator(weight_df, atomic_df)
        df_mahala_prop <- gen_RAA(RAL_data, "propto", "mahala")
        df_mahala_sel <- gen_RAA(RAL_data, "select_best", "mahala")
        df_agg <- rbind(df_agg, df_mahala_prop, df_mahala_sel)

    }

    return(df_agg)
}