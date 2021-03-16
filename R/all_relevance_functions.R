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
            if (sum((sotw[t == (i - 1), -1] - sotw[t == (k - 1), -1])^2) < tol) {
                similarity_df[j, 3] <- 1
            } else {
                similarity_df[j, 3] <- 0
            }
        }
        if (woc == "full") {
            if (sum(subset(similarity_df, similarity_df$t == i)$similarity) == 0) {
                stop(paste(
                    "Time point with zero observations within caliper",
                    "detected while using woc = full. Please increase",
                    "tolerance."
                ))
            }
        }
    }

    similarity_df <- data.table::data.table(similarity_df)
    similarity_df <- similarity_df[order(-t, -t2), .(t2, similarity = similarity / sum(similarity)), by = .(t)]
    return(similarity_df)
}

#' @title Relevance assessment method: Mahalanobis
#'
#' @description Calculates weights based on the Mahalanobis method.
#'
#' @param atomic_df Data frames with agent predictions.
#' @param sotw Data frame containing the state of the world at each time point.
#'   The first column of this data frame should be t (as in time).
#' @param start_agg From which value of t to start aggregating
#'
#' @import data.table

mahala_relevance <- function(atomic_df, sotw, start_agg) {
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
            md <- sqrt(sum((sotw[t == (i - 1), -1] - sotw[t == (k - 1), -1])^2))
            similarity_df[j, 3] <- 1 / md
        }
    }

    similarity_df <- data.table::data.table(similarity_df)
    similarity_df <- similarity_df[order(-t, -t2), .(t2, similarity = similarity / sum(similarity)), by = .(t)]
    return(similarity_df)
}