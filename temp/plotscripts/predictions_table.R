# Table of predictive ability (total) for different aggregation methods
# for all three dependent variables

library(xtable)
library(data_table)
library(devtools)
load_all()

# Run the code in predictions_final three times, one for each response
# then save after the line dff <- rbind (around line 164), with the 
# code below

# aggpred_gdp <- dff
# aggpred_gdp$outc <- "gdp"

# aggpred_tcpi <- dff
# aggpred_tcpi$outc <- "tcpi"

#aggpred_fed <- dff
#aggpred_fed$outc <- "fed"

pred_table_data <- rbind(
    aggpred_gdp,
    aggpred_tcpi,
    aggpred_fed
)

pred_table_data <- data.table(pred_table_data)

# Rename the levels


df_alpha <- pred_table_data[
    ,
    .(total_predabil = sum(lpdens)),
    .(method, outc)]

df_beta <- dcast(
    df_alpha,
    method ~ outc,
    value.var = "total_predabil"
)

# Better names
df_beta[1, 1] <- "BART"
df_beta[2, 1] <- "BVAR"
df_beta[3, 1] <- "SVBVAR"
df_beta[4, 1] <- "TVPSV"
df_beta[5, 1] <- "Local pool"
df_beta[6, 1] <- "Equal weight"
df_beta[7, 1] <- "Linear pool"


# LaTeX-ready output using xtable
print(
    xtable(df_beta),
    floating = FALSE,
    latex.environments = NULL,
    booktabs = TRUE
)