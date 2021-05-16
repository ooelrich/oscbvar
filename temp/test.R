
library(dplyr)
library(ggplot2)
library(devtools)
library(data.table)
load_all()
load("temp/pooling_vars.Rdata")


widths <- 1:50
datasets <- list(atomdat_1, atomdat_2, atomdat_3)
resps <- c("gdp", "gdptpi", "fedfunds")
for (j in 1:3) {
    for (i in 1:5) {
        i <- i
        aggpred_data <- gen_agg_preds(
            datasets[[j]],
            start_agg = 161,
            sotw = data.frame(pooling_vars[, c  (1:4, 9)]),
            baseline = TRUE,
            caliper = TRUE,
            mahala  = FALSE,
            cw = i,
            mvc = 10
        )
        # Doing the old classics evaluation
        aggpreds <- ggplot(aggpred_data, aes(y = lpdens, x = t, col = method)) +
            geom_line() +
            labs(title = sprintf("agg preds for response %s and cw %f", resps[j], i))
        ggsave(sprintf("temp/aggpreds/aggpreds_resp_%s_cw%f.pdf", resps[j], i), aggpreds)
    }
}



