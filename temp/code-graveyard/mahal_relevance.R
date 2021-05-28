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

    sim_df <- data.frame(matrix(ncol = 3, nrow = rows))
    x <- c("t", "t2", "similarity")
    colnames(sim_df) <- x
    sotw <- data.table::data.table(sotw)

    j <- 0
    for (i in start_agg:T) {
        for (k in start:(i - 1)) {
            j <- j + 1
            sim_df[j, 1] <- i
            sim_df[j, 2] <- k
            md <- sqrt(sum((sotw[t == (i - 1), -1] - sotw[t == (k - 1), -1])^2))
            sim_df[j, 3] <- 1 / md
        }
    }

    sim_df <- data.table::data.table(sim_df)
    sim_df <- sim_df[order(-t, -t2), .(t2, similarity = similarity / sum(similarity)), by = .(t)]
    return(sim_df)
}