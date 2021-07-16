library(data.table)

###################################
# Set up the data for the bikes ###
###################################

load("data-raw/sv_pred.Rdata")
load("data-raw/reg_pred.Rdata")
df_bikes <- rbind(sv_pred[141:nrow(sv_pred), ], reg_pred)
df_sotw <- subset(bikes_d, select = -c(cnt, sandy1, sandy2, cnt_l))
#df_scale <- scale(subset(df_sotw, select = -c(t)))
#df_sotw <- cbind(t = df_sotw$t, df_scale)
#df_sotw[, 2:8] <- scale(df_sotw[, 2:8])

df_sotw <- subset(bikes_d, select = -c(cnt, sandy1, sandy2, cnt_l, yr, workingday, weather_1, weather_2, mnth))


test_dist <- function() {
    i <- sample(nrow(df_sotw), 1)
    j <- sample(nrow(df_sotw), 1)
    dista <- sum((df_sotw[i, -1] - df_sotw[j, -1])^2)
    return(dista)
}

aa <- replicate(1e5, test_dist())
hist(aa, breaks = 100)

aggdata_list <- list()

for (i in seq_len(2)) {
    cwx <- i/100
    aggpreds <- gen_agg_preds(
        df_bikes,
        start_agg = 680,
        sotw = df_sotw,
        baseline = TRUE,
        caliper = TRUE,
        mahala = FALSE,
        cw = cwx,
        mvc = 1
    )

    cw <- rep(i, nrow(aggpreds))
    df <- cbind(aggpreds, cw)
    aggdata_list[[i]] <- df    
}

df_all <- do.call(rbind, aggdata_list)
df_all <- data.table(df_all)


# summary as table, find the optimal cw
df_all[, .(total_pred = sum(lpdens)), .(method, cw)][1:100,]


# select a specific (the best) cw and then throw away that varible
aggpreds <- subset(df_all, df_all$cw ==)
aggpreds <- subset(aggpreds, select = -c(cw))


# summary by plotting
# graying out the basilines
aggpreds$cat <- 1
for (i in seq_len(nrow(aggpreds))){
    if (aggpreds$method[i] == "equal_wt") {
        aggpreds$cat[i] <- 2
    }
    if (aggpreds$method[i] == "gewisano") {
        aggpreds$cat[i] <- 3
    }
    if (aggpreds$method[i] == "caliper_propto") {
        aggpreds$cat[i] <- 4
    }
}
aggpreds$cat <- factor(aggpreds$cat)

myplot <- ggplot(aggpreds, aes(y = lpdens, x = t, group = method, col = cat)) +
    geom_line(position = position_dodge(width = 0.5), size = 0.7) +
    theme_classic() + 
    labs(
        y = "Log predictive density",
        color = "Method"
    ) 
