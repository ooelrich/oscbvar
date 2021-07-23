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

sotw_all <- cbind(
    subset(bikes_d_log, select = c(t, temp, hum, windspeed)),
    time = bikes_d_log$t / sqrt(var(bikes_d_log$t))
)

# Ghettofix av sjuka resultat, sätt alla modellers lpdens till noll
# på den brutala sandy-dagen.

# farliga data-table modifikationer
df_atom[df_atom$t == 667, "lpdens"] <- 0


#############################################################
### GENERATE BASELINE PREDS AND A COLLECTION OF DIFFERENT ###
### CALIPER AGGREGATIONS                                  ###
#############################################################

df_agg_base <- gen_agg_preds(
    atomic_df = df_atom,
    start_agg = 401,
    sotw = sotw_aa,
    baseline = TRUE,
    caliper = FALSE,
    mahala = FALSE,
    cw = 0.01,
    mvc = 1
)

# add a cw column that is just NA for the baseline
df_agg_base <- cbind(df_agg_base, calw = NA)
df_all <- df_agg_base

cwl <- seq(0.03, 0.11, by = 0.04)
aaa <- Sys.time()
for (i in seq_len(length(cwl))) {
    cw <- cwl[i]
    df_agg <- gen_agg_preds(
        atomic_df = df_atom,
        start_agg = 401,
        sotw = sotw_all,
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

head(df_all)
df_all <- cbind(df_all, group = rep(1:3, each = 110))
dft <- df_all # safety save stupid

dft$method <- apply( dft[ , c(3, 6)], 1, paste, collapse = "_") # to that grouping works
data.table(dft[, .(meanlpdens = mean(lpdens)), by = .(method)])

ggplot(dft, aes(y = lpdens, x = t, color = method)) +
    geom_line() +
    facet_wrap(~group, ncol = 1, scales = "free") +
    labs(
        title = "Log pred density smoothing with all the fixings",
        x = "Time",
        y = "lpdens"
    )
ggplot2::ggsave("temp/aggpreds-all.pdf")
