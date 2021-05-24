library(dplyr)
library(ggplot2)
library(gghighlight)
library(devtools)
library(data.table)
load_all()
load("plotscripts/plt-data/pooling_vars.Rdata")


summary <- list()
datasets <- list(atomdat_1, atomdat_2, atomdat_3)
resps <- c("gdp", "gdptpi", "fedfunds")
for (j in 3) {
    for (i in 1:400) {
        print(i)
        cwi <- i/10
        aggpred_data <- gen_agg_preds(
            datasets[[j]],
            start_agg = 173,
            sotw = data.frame(pooling_vars[, c  (1:4, 9)]),
            baseline = TRUE,
            caliper = TRUE,
            mahala  = FALSE,
            cw = cwi,
            mvc = 10
        )
        # Doing the old classics evaluation
        #aggpreds <- ggplot(aggpred_data, aes(y = lpdens, x = t, col = method)) +
        #    geom_line() +
        #    labs(title = sprintf("agg preds for response %s and cw %f", resps[j], i))
        #ggsave(sprintf("temp/aggpreds/aggpreds_resp_%s_cw%f.pdf", resps[j], i), aggpreds)
        summary[[i]] <- aggpred_data[, .(mean_PA = mean(lpdens)), by=.(method)]
    }
}


val <- c()
cw <- c()
for (i in 1:length(summary)) {
    val[i] <- summary[[i]][3, 2]
    cw[i] <- i
}

plot(
    cw,
    val,
    type = "l",
    ylab = "mean_lpdens",
    xlab = "caliper width x5",
    main = "Predicting GDP"
)
