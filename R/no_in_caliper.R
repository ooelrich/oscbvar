#' @title Count number of previous observation in caliper over time
#'
#' @description
#' Description goes here.
#'
#' @details
#' Details go here.
#' 
#' @param cw Caliper width value.
#' @param df Data frame containing the pooling variables. The first
#'   column needs to be called "t" and contain the timepoint.
#' @param first_t Timepoint ("t") from which to start counting.
#' @param percentage Should the number of observations within the 
#'   caliper (FALSE) or the percentage (TRUE) be returned?
#'
#' @return A data.frame with columns count (numer of observation within
#'   caliper), cw_val (caliper width), and time (time point).
no_in_caliper <- function(cw, df, first_t, percentage) {
    
    stopifnot(first_t > min(df$t))
    stopifnot(colnames(df)[[1]] == "t") # First column should be "t"

    dftemp <- data.frame()
    last_t <- max(df$t)
    count <- cw_val <- time <- rep(NA, times = last_t + 1 - first_t)

    for (i in first_t:last_t) {
        count[i+1-first_t] = sum(
            rowSums(
                t(t(df[df$t < i, -1]) -
                unlist(c(df[df$t == i, -1])))^2
            ) < cw)
        if (percentage) {
            count[i+1-first_t] <- count[i+1-first_t] / (i - min(df$t))
        }
        cw_val[i+1-first_t] <- cw
        time[i+1-first_t] <- i
    }

    dftemp <- rbind(dftemp, data.frame(count, cw_val = as.factor(cw_val), time))
    return(dftemp)
}

#' @title Plot number of previous observation in caliper over time
#'
#' @description
#' Description goes here.
#'
#' @details
#' Details go here.
#'
#' @param cw_list List or vector of caliper width value.
#' @param df Data frame containing the pooling variables. The first
#'   column needs to be called "t" and contain the timepoint.
#' @param first_t Timepoint ("t") from which to start counting.
#' @param percentage Should the number of observations within the 
#'   caliper (FALSE) or the percentage (TRUE) be returned?
#'
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap labs
#' @importFrom rlang .data
plot_no_in_caliper <- function(cw_list, df, first_t, percentage = TRUE){

    df_list <- lapply(cw_list, no_in_caliper, df, first_t, percentage)
    df_fin <- do.call(rbind, df_list)
    if (ncol(df) > 2) {
        titt <- paste(colnames(df[, -1]), collapse = ",")    
    } else {
        titt <- colnames(df)[2]
    }
    

    if (length(levels(df_fin$cw_val)) < 6) {
        plt <- ggplot(df_fin, aes(x = .data$time, y = .data$count, col = .data$cw_val)) +
            geom_line() +
            labs(title = titt)
    } else {
        plt <- ggplot(df_fin, aes(x = .data$time, y = .data$count)) +
            geom_line() +
            facet_wrap( ~ .data$cw_val) +
            labs(title = titt)
    }

    return(plt)
}