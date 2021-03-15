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
    df_equal_wt <- df_all[t >= start_agg,
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
        
        df_RAL <- data[, .(pmean = sum(pmean * exp(RAL)) / sum(exp(RAL)),
                           lpdens = log(sum(exp(lpdens)*exp(RAL)/sum(exp(RAL)))),
                           method = "RAL_propto", t), by = .(t)][, -1]
        return(df_RAL)
}


#' @title Weighting by picking the best model
#' 
#' @param data Which dataset to use
#' 
#' @import data.table

selbest_weighting <- function(data) {
    df_RAL <- data[, .(pmean = max(pmean),
                       lpdens =max(lpdens),
                       method = "RAL_selbest", t), by = .(t)][, -1]
    return(df_RAL)
}

#' @title Relevance adjusted logscore calculator
#' 
#' @description Calculates the relevance adjusted logscores given a weight data
#'   frame and a data frame of atomic predictions.
#' 
#' @param weight_df Data frame containing relevance adjusted weights. Generated
#'   by a relevance function (caliper relevance or mahalanobis atm).
#' @param atomic_df Data frame with atomic predictions from any number of models.
#' 
#' @import data.table

RAL_calculator <- function(weight_df, atomic_df) {
    
    start_agg <- min(weight_df$t)
    stop_agg <- max(weight_df$t)

    atomic_df <- data.table::data.table(atomic_df)
    
    dfdf <- data.table::data.table(matrix(ncol = 3, nrow = 0)) # df to store RAL
    colnames(dfdf) <- c("method", "RAL", "t")
    
    for (i in start_agg:stop_agg) {
        sim_df <- weight_df[t == i, .(t = t2, similarity)]
        atomic_sub <- atomic_df[t < i, .(lpdens, method, t)]
        data.table::setkey(sim_df, t)
        data.table::setkey(atomic_sub, t)
        df_all <- atomic_sub[sim_df]
        df_all[, `:=`(adj_lpdens = lpdens * similarity)]
        collapsed <- df_all[, .(RAL = sum(adj_lpdens)), by = .(method)]
        collapsed[, `:=`(t = i)]
        dfdf <- rbind(dfdf, collapsed)
    }

    keycols <- c("method", "t")
    data.table::setkeyv(dfdf, keycols)
    data.table::setkeyv(atomic_df, keycols)
    joined_df <- atomic_df[dfdf]
    return(joined_df)
}

#' @title Generate GewiSano Weights
#' 
#' @description
#' Generates weights according to Geweke & Amisano 2011/2012, also known as
#' linear predicition pools.
#' 
#' @param data Data set of atomic predictions.
#' @param start_t Which timepoint to start generating weights for. Obs! This is
#'   based on the variable t in the data, not the row number!
#' @import pracma, data.table

gen_gewisano <- function(data, start_t) {

    fun_to_opt <- function(x, dataopt) {
        -sum(log(exp(dataopt) %*% x))
    }

    data <- data.table::data.table(data)
    df_fat <- data.table::dcast(data, t ~ method, value.var = "lpdens")
    k <- ncol(df_fat) - 1
    start_val <- rep((1 / k), k)
    tt <- seq(start_t, max(data$t))
    wts <- matrix(ncol = k, nrow = length(tt))

    j <- 0
    for (i in start_t:(max(data$t))) {
        j <- j + 1
        vecop <- as.matrix(df_fat[df_fat$t <= i, 2:(k + 1)])
        opt_sol <- pracma::fmincon(start_val, fun_to_opt, Aeq = matrix(1, 1, k),
                beq = 1, lb = rep(0, k), ub = rep(1, k), dataopt = vecop)
        wts[j, ] <- opt_sol$par
    }

    lpdens <- log(rowSums(exp(df_fat[df_fat$t >= start_t, 2:(k + 1)]) * wts))
    df_pmean <- data.table::dcast(data, t ~ method, value.var = "pmean")
    pmean <- rowSums(df_pmean[df_pmean$t >= start_t, 2:(k + 1)] * wts)
    
    method <- rep("gewisano", length(tt))
    df_gew_res <- data.frame(pmean, lpdens, method, tt)
    colnames(df_gew_res) <- c("pmean", "lpdens", "method", "t")
    df_gew <- gen_atomic_df()
    df_gew <- rbind(df_gew, df_gew_res)

    return(df_gew)
}