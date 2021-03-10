#' @title Relevance assessment method: Caliper
#' 
#' @description 
#' Calculates weights for which previous log scores to use based on the 
#' relevance adjusted caliper method. All observations within a certain
#' distance are given equal weight, all outside of that distance are given
#' zero weight.
#' 
#' @param atomic_df Data frames with agent predictions.
#' @param start_agg From which value of t to start aggregating
#' @param tol Determines how similar observations need to be to be included.
#' @param sotw Data frame containing the state of the world at each time point.
#'   The first column of this data frame should be t (as in time).
#' 
#' @import data.table

caliper_relevance <- function(atomic_df, start_agg, tol, sotw) {
    
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
    }

    similarity_df <- data.table::data.table(similarity_df)
    similarity_df <- similarity_df[order(-t, -t2), .(t2, similarity = similarity/sum(similarity)), by = .(t)]
    return(similarity_df)
}