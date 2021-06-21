# Generate a bunch of datasets of aggregate predictions, for a
# selection of different cw values

# the values of cw will go from 1/cwx to max(loopval)/cwx in increments
# of 1/cwx. Change the max in the for-loop and the cwx to change stuff

library(devtools)
load_all()

##################################
### USER INPUT                 ###
##################################

# Select one of the following

dfx <- atomdat_3 # for FEDFUNDS
outc <- "fed"

dfx <- atomdat_2 # for GDPTCPI
outc <- "tcpi"

dfx <- atomdat_1 # for GDP
outc <- "gdp"

###################################
###################################
###################################


load("data-raw/pooling_vars.Rdata")

aggdata_list <- list()
for (i in 1:400) {

    print(i) # ghetto-timer
    cwx <- i/10
    aggpred_data <- gen_agg_preds(
        dfx,
        start_agg = 173,
        sotw = data.frame(pooling_vars[, c  (1:4, 9)]),
        baseline = TRUE,
        caliper = TRUE,
        mahala  = FALSE,
        cw = cwx,
        mvc = 10
    )

    cw <- rep(i, nrow(aggpred_data))
    df <- cbind(aggpred_data, cw)

    aggdata_list[[i]] <- df

}

df_all <- do.call(rbind, aggdata_list)
df_all <- data.table(df_all)

filtit <- sprintf("data-raw/data_allcw_%s.rds", outc)

saveRDS(df_all, file = filtit)