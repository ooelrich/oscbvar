### Version for FEDFUNDS
# This code just does the aggregation for a bunch of different cw
# values (from 1 to 40), and then stacks those data frames with cw
# as a new varible. We can then go in and pick out the relationship
# between cw and lpdens for different time points easily with
# data.table

library(ggplot2)
library(devtools)
load_all()

aggdata_list <- list()
for (i in 1:40) {

    print(i) # ghetto-timer

    aggpred_data <- gen_agg_preds(
        atomdat_3,
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
for (i in 173:214) {

    cw_data <- df_all[
        method == "caliper_propto" & t == i,
        .(pred_abil = lpdens, time = i, calw = cw)
    ]

    temp_list[[i - 172]] <- cw_data

}

all_things <- do.call(rbind, temp_list)

cwplot <- ggplot(all_things, aes(x = calw, y = pred_abil)) +
    geom_line() +
    facet_wrap(~time, scales = "free")
ggsave("temp/plt_cw_lpdens_over_time_fed.pdf", cwplot)


# Cumulative version

temp_list <- list()
for (i in 173:214) {

    cw_data <- df_all[
        method == "caliper_propto" & t <= i,
        .(pred_abil = sum(lpdens), time = i, calw = cw),
        .(cw)
    ]

    temp_list[[i - 172]] <- cw_data

}

all_things <- do.call(rbind, temp_list)

cwplot_cumulative <- ggplot(all_things, aes(x = calw, y = pred_abil)) +
    geom_line() +
    facet_wrap(~time, scales = "free")
ggsave("temp/plt_cw_lpdens_over_time_cumulat_fed.pdf", cwplot_cumulative)

