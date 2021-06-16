# This code uses the aggregation for a bunch of different cw
# values (from 1 to 40), and then stacks those data frames with cw
# as a new varible. We can then go in and pick out the relationship
# between cw and lpdens for different time points easily with
# data.table
# Select one of three dependent variables under user input, then run
# the rest of the code to generate two plots (on cumulative, one not)

######################################################################
### USER INPUT  ######################################################
######################################################################

# Pick a data set, any data ste
df_cw_fed <- readRDS("data-raw/data_allcw_fed.rds")
df_all <- df_cw_fed
outc <- "fed"

df_cw_tcpi <- readRDS("data-raw/data_allcw_tcpi.rds")
df_all <- df_cw_tcpi
outc <- "tcpi"

df_cw_gdp <- readRDS("data-raw/data_allcw_gdp.rds")
df_all <- df_cw_gdp
outc <- "gdp"

######################################################################
######################################################################

library(ggplot2)
library(devtools)
load_all()

# Make a list where each element shows the relationship between lpdens
# and t at a given time point
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
plot_tit <- sprintf("temp/cw_vs_lpdens_history_%s.pdf", outc)
ggsave(plot_tit, cwplot)


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
plot_tit <- sprintf("temp/cw_vs_lpdens_cumulative_%s.pdf", outc)
ggsave(plot_tit, cwplot_cumulative)