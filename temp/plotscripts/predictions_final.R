# First create a data table with cumulative caliper width vs lpdens.
# Next, all we need to do is to select the caliper width corresponding
# to the largest value. Note that at time t, pred_abil is the sum of
# all previous predictions.

#####################################################################
### USER INPUT ######################################################
#####################################################################

# FED version
df_all <- readRDS("data-raw/data_allcw_fed.rds")
outc <- "fed"
dfx <- atomdat_3

# Interest version
df_all <- readRDS("data-raw/data_allcw_tcpi.rds")
outc <- "tcpi"
dfx <- atomdat_2

# GDP version
df_all <- readRDS("data-raw/data_allcw_gdp.rds")
outc <- "gdp"
dfx <- atomdat_1

#####################################################################
#####################################################################

library(dplyr)
library(ggplot2)
library(gghighlight)
library(devtools)
library(data.table)
load_all()

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
        dfx,
        start_agg = 174,
        sotw = data.frame(pooling_vars[, c  (1:4, 9)]),
        baseline = TRUE,
        caliper = TRUE,
        mvc = 10,
        calip_data = opt_cal
)

### "NYA" KODEN BÖRJAR HÄR
dff <- rbind(dfx[dfx$t > 173, ], aggpred_data)

# aggpred_gdp <- dff
# aggpred_gdp$outc <- "gdp"

# aggpred_tcpi <- dff
# aggpred_tcpi$outc <- "tcpi"

#aggpred_fed <- dff
#aggpred_fed$outc <- "fed"

# This should be done with a lookup table
dff$cat <- 1
for (i in seq_len(nrow(dff))){
    if (dff$method[i] == "equal_wt") {
        dff$cat[i] <- 2
    }
    if (dff$method[i] == "gewisano") {
        dff$cat[i] <- 3
    }
    if (dff$method[i] == "caliper_propto") {
        dff$cat[i] <- 4
    }
}
dff$cat <- factor(dff$cat)

aggpreds <- ggplot(
        dff,
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
dff <- data.table(dff)
df_lab <- dff[t == 214] # select the last point, (t, elpd) will be (x,y)
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
names(my_colors) <- levels(factor(c(levels(dff$cat), levels(df_lab$cat)))) 
my_scale <- scale_color_manual(name = "cat", values = my_colors)
final <- final + my_scale  

# Finally, fixing the x-axis
final <- final + scale_x_continuous(
    name = "",
    breaks = c(178, 190, 202, 214),
    labels = c("2010Q1", "2013Q1", "2016Q1", "2019Q1")
)

plottit <- sprintf("temp/final_%s.pdf", outc)
ggsave(plottit, final)

# The you should run pdfcrop fedfunds_final.pdf fedfunds_final.pdf
# in the terminal to get a version that doesn't look ridiculous