#' @title Generate similarity data table
#' 
#' @description Generates a data table used by the DM to determine relevance
#'   adjusted log scores.
#' 
#' @param method Which relevance assessment method should be used?
#' @param sotw State of the world data frame supplied by the DM. 
#' @param start_agg 

gen_similarity_table <- function(method, sotw, start_agg,...) {
    sim_df <- switch(method,
                    "caliper" = (sotw),
                    "mahala" = (sotw),
                    stop("Unknown relevance assessment method."))
    return(sim_df)
}


#' @title Relevance assessment method: Caliper
#' 
#' @description 
#' Calculates weights for which previous log scores to use based on the 
#' relevance adjusted caliper method. All observations within a certain
#' distance are given equal weight, all outside of that distance are given
#' zero weight.
#' 
#' @details 
#' A potential problem with the caliper method is that we don't really know
#' how many observations are going to end up within the tolerance. I have no
#' idea what is going to turn out as a "reasonable" number, but clearly having 
#' very few (1-5 say) observations would make the variance of the RAL very high.
#' 
#' @param atomic_df Data frames with agent predictions.
#' @param sotw Data frame containing the state of the world at each time point.
#'   The first column of this data frame should be t (as in time).
#' @param start_agg From which value of t to start aggregating
#' @param tol Determines how similar observations need to be to be included.
#' @param woc Weight on caliper. This specifies how the observations "within"
#'   the caliper should be weighted. Takes vaules "full" meaning that only 
#'   observations inside the tolerance count, or "progressive", which gives 
#'   weight to the observations within the caliper proportional to the number of
#'   observations within the caliper. Not that if the "full" method is being
#'   used and error will be thrown if some timepoints have zero previous 
#'   observations within the caliper.
#' 
#' @import data.table

caliper_relevance <- function(atomic_df, sotw, start_agg, tol = 5, woc = "full") {
    
    T <- max(atomic_df$t)
    start <- min(atomic_df$t)
    p <- (T - start_agg + 1)
    rows <- sum(seq_len(p))

    similarity_df <- data.frame(matrix(ncol = 3, nrow = rows))
    x <- c("t", "t2", "similarity")
    colnames(similarity_df) <- x
    sotw <- data.table::data.table(sotw)

    j <- 0
    for (i in start_agg:T) {
        for (k in start:(i - 1)) {
            j <- j + 1
            similarity_df[j, 1] <- i
            similarity_df[j, 2] <- k
            if(sum((sotw[t == (i - 1), -1] - sotw[t == (k - 1), -1])^2) < tol) {
                similarity_df[j, 3] <- 1
            } else {
                similarity_df[j, 3] <- 0
            }
        }
        if (woc == "full"){
            if (sum(subset(similarity_df, similarity_df$t == i)$similarity) == 0) {
                stop(paste("Time point with zero observations within caliper", 
                           "detected while using woc = full. Please increase",
                           "tolerance."))
            }
        }
        
    }

    similarity_df <- data.table::data.table(similarity_df)
    similarity_df <- similarity_df[order(-t, -t2), .(t2, similarity = similarity/sum(similarity)), by = .(t)]
    return(similarity_df)
}

#' @title Relevance assessment method: Mahalanobis
#' 
#' @description 
#'
#' @param atomic_df Data frames with agent predictions.
#' @param sotw Data frame containing the state of the world at each time point.
#'   The first column of this data frame should be t (as in time).
#' @param start_agg From which value of t to start aggregating

mahala_relevance <- function(atomic_df, sotw, start_agg, tol = 5, woc = "full") {

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
#' @imports data.table

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