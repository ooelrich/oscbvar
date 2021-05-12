#' @title Relevance assessment method: Caliper
#'
#' @description
#' Calculates weights for which previous log scores to use based on the
#' caliper method. 
#'
#' @details
#' The caliper method splits the estimate between a local and a global
#' part. The local part is the average of all log scores within the 
#' caliper width, while the global part is the global average. The
#' balance between the global and local part depends on the minimum
#' viable clusester size. If the number of observations within the
#' caliper equals or exceeds the minimum viable cluster size, the
#' global estimates gets zero weight. When there are no obsevations 
#' within the cluster, the global estimate gets all the weight. For all
#' situations between these extremes, a linear combination depending on
#' how large a percentage of the minimum viable cluster size is
#' attained. (Se paper for maths.)
#'
#' @param atomic_df Data frames with agent predictions.
#' @param sotw Data frame containing the state of the world at each
#'   time point, which can include decision maker variables no in any
#'   of the atomic models. The first column of this data frame should
#'   be t (as in time).
#' @param start_agg From which value of t to start aggregating, ie 
#'   producing aggregate predictions.
#' @param cw The caliper width.
#' @param mvc Minimum viable cluster size.
#'
#' @import data.table

caliper_relevance <- function(
        atomic_df,
        sotw,
        start_agg = 161,
        cw = 5,
        mvc = 1
) {
    
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
            if (sum((sotw[t == (i - 1), -1] - sotw[t == (k - 1), -1])^2) < cw) {
                sim_df[j, 3] <- 1
            } else {
                sim_df[j, 3] <- 0
            }
        }
    }

    sim_df <- data.table::data.table(sim_df)
    sim_df <- sim_df[
        order(-t, -t2), 
        .(t2, similarity = 
            similarity * 
            (
                1/sum(similarity) -
                max((mvc - sum(similarity)) / mvc, 0) *
                    (1 / sum(similarity - 1 / .N))
            ) + 
            (1 - similarity) * 
                max((mvc - sum(similarity)) / mvc, 0) / .N),
        by = .(t)
    ]

    return(sim_df)
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