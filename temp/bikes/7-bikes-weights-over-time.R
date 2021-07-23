############################################################
### EXPLORING HOW MODEL WEIGHTS VARY OVER TIME           ###
############################################################

# First we need the atomic predictions
load("data-raw/bikes_bart.RData")
load("data-raw/bikes_sv.RData")
load("data-raw/bikes_reg.RData")
df_atom <- rbind(bikes_bart, bikes_sv, bikes_reg)

############################################################
### ONLY TIME AS SMOOTHING VARIABLE                      ###
############################################################

sotw_time <- data.frame(
    t = bikes_d_log$t,
    time = bikes_d_log$t/sqrt(var(bikes_d_log$t))
)

# Then we need to jump through some hoops
weight_df <- caliper_relevance(
    atomic_df = df_atom,
    sotw = sotw_time,
    start_agg = 401,
    cw = 0.01,
    mvc = 1
)

df_wt <- RAL_calculator(weight_df, df_atom)

weights <- df_wt[, .(method,wegh = exp(RAL)/sum(exp(RAL))), by = .(t)]

ggplot(weights, aes(x = t, y = wegh, col = method)) + geom_line()
ggsave("temp/weights_cw_001.pdf")


############################################################
### ONLY THE CONTINUOUS  AS SMOOTHING VARIABLES          ###
############################################################

sotw_cont <- subset(bikes_d_log, select = c(t, temp, hum, windspeed))


############################################################
### TIME PLUS CONT. AS SMOOTHING VARIABLES               ###
############################################################

sotw_all <- cbind(
    subset(bikes_d_log, select = c(t, temp, hum, windspeed)),
    time = bikes_d_log$t / sqrt(var(bikes_d_log$t))
)
