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

#' @title Generate relevance adjusted aggregations
#' 
#' @description Generates relevance adjusted aggregations given a data set with
#'   relevance adjusted logscores.
#' 
#' @param RAL_data Data set containing relevance adjusted logscores.
#' @param agg_meth Which method should be used to aggregate the agents based on
#'   their relevance-adjusted logscores. Currently available are propto (which
#'   uses a softmax transformation) and select_best which picks the model with 
#'   the best RAL at each time point.
#' @import data.table

gen_RAA <- function(RAL_data, agg_meth) {

    # Using the method of giving each model the same RAL when NA
    # This will happen when using caliper and there is no relevant data
    RAL_data[is.na(RAL_data)] <- 1

    df_RAL <- switch(agg_meth,
                    "propto" = propto_weighting(RAL_data),
                    "select_best" = selbest_weighting(RAL_data),
                    stop("Unknown aggregation method."))
    
    return(df_RAL)
}


#' @title Weighting proportional to RAL
#' 
#' @param data Dataset to use
#' 
#' @import data.table

propto_weighting <- function(data) {
        
        df_RAL <- data[, .(lpdens = log(sum(exp(lpdens)*exp(RAL)/sum(exp(RAL)))),
                            pmean = sum(pmean * exp(RAL)) / sum(exp(RAL)),
                            method = "RAL_propto", t), by = .(t)][, -1]
        return(df_RAL)
}


#' @title Weighting by picking the best model
#' 
#' @param data Which dataset to use
#' 
#' @import data.table

selbest_weighting <- function(data) {
    df_RAL <- data[, .(lpdens =max(lpdens),
                       pmean = max(pmean),
                       method = "RAL_selbest", t), by = .(t)][-1]
    return(df_RAL)
}