
library(dplyr)
library(ggplot2)
library(devtools)
library(data.table)
load_all()




weight_df <- caliper_relevance(
    atomdat_3,
    sotw,
    161,
    5,
    10
)

View(weight_df)

head(sotw)
sotw <- sotw[, c(1,2,3,4,9)]
head(sotw)


aggpred_data <- gen_agg_preds(
    atomdat_2,
    start_agg = 161,
    sotw,
    baseline = TRUE,
    caliper = TRUE,
    mahala  = FALSE,
    cw = 5,
    mvc = 10
)
