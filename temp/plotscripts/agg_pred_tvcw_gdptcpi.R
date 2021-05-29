# First create a data table with cumulative caliper with vs lpdens.
# Next, all we need to do is to select the caliper width corresponding
# to the largest value. Note that at time t, pred_abil is the sum of
# all previous predictions.

# First chunk takes 8 minutes to run!

aggdata_list <- list()
for (i in 1:40) {

    print(i) # ghetto-timer

    aggpred_data <- gen_agg_preds(
        atomdat_2,
        start_agg = 173,
        sotw = data.frame(pooling_vars[, c  (1:4, 9)]),
        baseline = TRUE,
        caliper = TRUE,
        mahala  = FALSE,
        cw = i,
        mvc = 10
    )

    cw <- rep(i, nrow(aggpred_data))
    df <- cbind(aggpred_data, cw)

    aggdata_list[[i]] <- df

}

df_all <- do.call(rbind, aggdata_list)
df_all <- data.table(df_all)


temp_list <- list()
for (i in 174:214) {

    cw_data <- df_all[
        method == "caliper_propto" & t < i,
        .(pred_abil = sum(lpdens), time = i, calw = cw),
        .(cw)
    ]

    temp_list[[i - 172]] <- cw_data

}

all_things <- do.call(rbind, temp_list)

# Create a data-set that gives us a caliper width for each time point
# To re-iterate, this cw is the cw that optimizes the predictions of
# all previous predictions.

opt_cal <- all_things[, .SD[which.max(pred_abil)], .(time)][,.(time, cw)]
colnames(opt_cal) <- c("t", "cw")

# Dynamic versions of the aggregate generation. Gotta refactor the code.

gen_agg_preds_dynamic <- function(
        atomic_df,
        start_agg,
        sotw,
        baseline = TRUE,
        caliper = TRUE,
        mvc = 10,
        calip_data
) {

    df_agg <- gen_atomic_df()
    

    if (baseline) {
        df_base <- gen_baseline(atomic_df, start_agg)
        df_agg <- rbind(df_agg, df_base)
    }

    if (caliper) {

        weight_df <- caliper_relevance_dynamic(
            atomic_df,
            sotw,
            start_agg,
            calip_data = calip_data,
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

    return(df_agg)
}

caliper_relevance_dynamic <- function(
        atomic_df,
        sotw,
        start_agg = 161,
        calip_data,
        mvc = 10
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
            # max sum of predabil BERFORE i:
            cw <- calip_data$cw[calip_data$t == i] 
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
            similarity + 
            (1 - similarity) * 
                max((mvc - sum(similarity)), 0) /
                ifelse(
                    .N - sum(similarity) > 0,
                    .N - sum(similarity),
                    1
                )
                ),
        by = .(t)
    ]

    return(sim_df)
}

aggpred_data <- gen_agg_preds_dynamic(
        atomdat_2,
        start_agg = 174,
        sotw = data.frame(pooling_vars[, c  (1:4, 9)]),
        baseline = TRUE,
        caliper = TRUE,
        mvc = 10,
        calip_data = opt_cal
)

df_gdptcpi <- rbind(atomdat_2[atomdat_2$t > 173, ], aggpred_data)

aggpreds <- ggplot(df_gdptcpi, aes(y = lpdens, x = t, col = method)) +
    geom_line() +
    theme_classic() + 
    labs(
        title = "Predictive ability of caliper v baseline versions",
        subtitle = "GDPTCPI. Dynamic caliper width.",
        x = "Time",
        y = "Log predictive density",
        color = "Method"
    ) 
    
final <- aggpreds +
    gghighlight(
        method %in% c("caliper_propto", "gewisano", "equal_wt"),
        use_direct_label = FALSE
    )


ggsave("temp/final_dynamic_gdptcpi.pdf", final)

# How well the methods do
df_gdptcpi <- data.table(df_gdptcpi)
df_gdptcpi[, .(mean_predab = mean(lpdens)), .(method)]