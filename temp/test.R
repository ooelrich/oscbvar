library(devtools)

load_all()

menu <- gen_atomic_list()
list_of_models <- menu[1:4]

first_half <- gen_atomic_preds(list_of_models)


sotw <- macrodata[5:nrow(macrodata), ]
sotw <- data.frame(t = c(1:nrow(sotw)), sotw)


weight_df <- caliper_relevance(
    first_half,
    sotw,
    161,
    5,
    "uncond"
)

View(weight_df)
RAL_data <- RAL_calculator(weight_df, first_half)
df_cal_prop <- gen_RAA(RAL_data, "propto", "caliper")
df_cal_sel <- gen_RAA(RAL_data, "select_best", "caliper")
df_agg <- rbind(df_agg, df_cal_prop, df_cal_sel)




# Experimenting with tiny matrices
t <- c(10, 10, 10, 9, 9, 9)
t2 <- c(9, 8, 7, 8, 7, 6)
targe <- rnorm(6)
snurre <- rnorm(6)
dft <- data.table(t, t2, targe, snurre)
dft[, sumsim := sum(simi), by = .(t)]
dft$simi[dft$sumsim == 0] <- 1
dft[, sumsim := NULL]

dft[targe == max(targe), .(t2) , by = .(t)]
dft2 <-dft[dft[, .I[targe == max(targe)], by = t]$V1]
dft
dft2[, method := "caliper"]

dft$snurre <- 1

# debuggin the actual function


# sim_measure is just caliper

RAL_data
hahah <- selbest_weighting(RAL_data, "caliper")
