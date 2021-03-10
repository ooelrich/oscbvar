#' @title Generate baseline aggregations
#' 
#' @description Generates baseline aggregations to compare with the relevance
#'   adjusted ones.
#' 
#' @details
#' Generates the following baseline aggregations: 
#' * Equal weight: gives equal weight to all models.
#' * Gewisano: gives weight using linear prediction pools (Geweke&Amisano).
#' 
#' @param atomic_df A data frame consisting of atomic predictions.
#' @param start_agg Which time point to start creating aggregate predictions for.
#'   Note that this is based on the variable t in the supplied data frame, and
#'   not row number.

gen_baseline <- function(atomic_df, start_agg) {

    baseline_df <- gen_atomic_df()
    T <- max(atomic_df$t)

    # Giving all models equal weight
    df_all <- data.table::data.table(atomic_df)
    df_equal_wt <- df_all[start_agg:T,
        .(pmean = mean(pmean), lpdens = log(mean(exp(lpdens))), method = "equal_wt", t),
        by = .(t)][, 2:5]
    baseline_df <- rbind(baseline_df, df_equal_wt)

    # Optimal prediction pools (Geweke & Amisano)
    df_gewisano <- gen_gewisano(data = atomic_df, start_agg)
    baseline_df <- rbind(baseline_df, df_gewisano)

    return(baseline_df)
}