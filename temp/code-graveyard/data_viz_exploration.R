library(oscbvar)
library(ggplot2)
library(reshape2)

## PREPARING DATA FOR SUPERPLOTTING
# scatterplot of each variable against the lags of
# each other variable

og <- macrodata[1:217, ]
og_lagged <- macrodata[2:218, ]
n_obs <- 217

lagged1 <- c(rep("GDPC1_lag1", n_obs),
            rep("GDPCTPI_lag1", n_obs),
            rep("FEDFUNDS_lag1", n_obs),
            rep("PCECC96_lag1", n_obs),
            rep("GPDIC1_lag1", n_obs),
            rep("HOANBS_lag1", n_obs),
            rep("AHETPIx_lag1", n_obs))

dfmelt <- melt(og)[, 2:3]
dfmelt_lag <- melt(og_lagged)[, 2:3]
dfmelt_lag$Var2 <- lagged1
colnames(dfmelt_lag) <- c("lagged_variable", "lagged_value")
colnames(dfmelt) <- c("variable", "value")
dfmelt_all <- do.call("rbind", replicate(7, dfmelt, simplify = FALSE))

df_tmp <- list()
for (i in 1:7) {
    df_tmp[[i]] <- do.call("rbind",
        replicate(7, dfmelt_lag[((i - 1) * 217 + 1):(217 * i), ],
        simplify = FALSE))
}
dfmelt_lag_all <- do.call("rbind", df_tmp)
df_full <- cbind(dfmelt_all, dfmelt_lag_all)

ggplot(data = df_full) + geom_point(aes(y = value, x = lagged_value)) +
    facet_grid(rows = vars(variable), cols = vars(lagged_variable),
                scales = "free")
