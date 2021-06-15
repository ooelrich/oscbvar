# First create a data table with cumulative caliper with vs lpdens.
# Next, all we need to do is to select the caliper width corresponding
# to the largest value. Note that at time t, pred_abil is the sum of
# all previous predictions.

# First part of this script takes a couple of minutes to run
# (5-10 or so)
# You probably want to rerun this with higher "resolution", ie over
# a finer grid of cw-values

library(dplyr)
library(ggplot2)
library(gghighlight)
library(devtools)
library(data.table)
load_all()

load("temp/plotscripts/plt-data/pooling_vars.Rdata")

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

opt_cal <- all_things[
    ,
    .SD[which.max(pred_abil)],
    .(time)
][, .(time, cw)]

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
        atomdat_3,
        start_agg = 174,
        sotw = data.frame(pooling_vars[, c  (1:4, 9)]),
        baseline = TRUE,
        caliper = TRUE,
        mvc = 10,
        calip_data = opt_cal
)


### "NYA" KODEN BÖRJAR HÄR
df_fed <- rbind(atomdat_3[atomdat_3$t > 173, ], aggpred_data)

# This should be done with a lookup table
df_fed$cat <- 1
for (i in seq_len(nrow(df_fed))){
    if (df_fed$method[i] == "equal_wt") {
        df_fed$cat[i] <- 2
    }
    if (df_fed$method[i] == "gewisano") {
        df_fed$cat[i] <- 3
    }
    if (df_fed$method[i] == "caliper_propto") {
        df_fed$cat[i] <- 4
    }
}
df_fed$cat <- factor(df_fed$cat)

aggpreds <- ggplot(
        df_fed,
        aes(y = lpdens, x = t, group = method, col = cat)) +
    geom_line(position = position_dodge(width = 0.5), size = 0.7) +
    theme_classic() + 
    labs(
        y = "Log predictive density",
        color = "Method"
    ) +
    xlim(174, 215) +
    theme(aspect.ratio = 7/16)

# Make data frame for the labels
df_fed <- data.table(df_fed)
df_lab <- df_fed[t == 214] # select the last point, (t, elpd) will be (x,y)
df_lab[7, 2] <- df_lab[7, 2] - 0.1  # Move caliper version down slighlty
df_lab[6, 2] <- df_lab[6, 2] + 0.05 # Move linear pool up a smidge

# Better names
df_lab[1, 3] <- "BVAR"
df_lab[2, 3] <- "SVBVAR"
df_lab[3, 3] <- "BART"
df_lab[4, 3] <- "TVPSV"
df_lab[5, 3] <- "Equal weight"
df_lab[6, 3] <- "Linear pool"
df_lab[7, 3] <- "Local pool"

final <- aggpreds + geom_text(
    df_lab,
    mapping = aes(x = t, y = lpdens, label = method, color = cat),
    nudge_x = 2.3,
    size = 2
) + theme(legend.position = "none")


my_colors <- c("grey", RColorBrewer::brewer.pal(3, "Dark2"))
names(my_colors) <- levels(factor(c(levels(df_fed$cat), levels(df_lab$cat)))) 
my_scale <- scale_color_manual(name = "cat", values = my_colors)
final <- final + my_scale  

# Finally, fixing the x-axis
final <- final + scale_x_continuous(
    name = "",
    breaks = c(178, 190, 202, 214),
    labels = c("2016Q1", "2017Q1", "2018Q1", "2019Q1")
)

ggsave("temp/fedfunds_final.pdf", final)

# The you should run pdfcrop fedfunds_final.pdf fedfunds_final.pdf
# in the terminal to get a version that doesn't look ridiculous