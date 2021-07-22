############################################################
### GENERATE AGGREGATE PREDICTIONS                       ###
############################################################
load("data-raw/bikes_bart.RData")
load("data-raw/bikes_sv.RData")
load("data-raw/bikes_reg.RData")
df_atom <- rbind(bikes_bart, bikes_sv, bikes_reg)

############################################################
### GENERATE A SMOOTHING FRAME, REMOVING WORST SANDY DAY ###
############################################################

sotw_time <- data.frame(
    t = bikes_d_log$t,
    time = bikes_d_log$t/var(bikes_d_log$t)
)

sotw_time <- subset(sotw_time, sotw_time$t != 667)
df_atom <- subset(df_atom, df_atom$t != 667)

sotw_time$t[sotw_time$t > 667] <- sotw_time$t[sotw_time$t > 667] - 1
df_atom$t[df_atom$t > 667] <- df_atom$t[df_atom$t > 667] - 1

#############################################################
### GENERATE BASELINE PREDS AND A COLLECTION OF DIFFERENT ###
### CALIPER AGGREGATIONS                                  ###
#############################################################

df_agg_base <- gen_agg_preds(
    atomic_df = df_atom,
    start_agg = 401,
    sotw = sotw_time,
    baseline = TRUE,
    caliper = FALSE,
    mahala = FALSE,
    cw = 0.01,
    mvc = 1
)

# add a cw column that is just NA for the baseline
df_agg_base <- cbind(df_agg_base, calw = NA)
df_all <- df_agg_base
head(df_all)

cwl <- seq(0.06, 0.2, by = 0.05)
cwl <- c(0.01, 2, 10)
aaa <- Sys.time()
for (i in seq_len(length(cwl))) {
    cw <- cwl[i]
    df_agg <- gen_agg_preds(
        atomic_df = df_atom,
        start_agg = 401,
        sotw = sotw_time,
        baseline = FALSE,
        caliper = TRUE,
        mahala = FALSE,
        cw = cw,
        mvc = 1
    )
    print(sprintf("done with cw %.2f", cw))
    df_agg <- cbind(df_agg, calw = cw)
    df_all <- rbind(df_all, df_agg)
}
Sys.time() - aaa
# Tog 50 minuter för tre olika cw

dim(df_all)
head(df_all)



df <- rbind(df_agg, df_agg_base)
df <- cbind(df, group = rep(1:2, each = 123))
data.table(df_all[, .(meanpred = mean(lpdens)), by = .(method, calw)])

ggplot(df, aes(y = lpdens, x = t, color = method)) +
    geom_line() +
    #facet_wrap(~group, ncol = 1, scales = "free") +
    labs(
        title = "Log pred density",
        x = "Time",
        y = "lpdens"
    )
