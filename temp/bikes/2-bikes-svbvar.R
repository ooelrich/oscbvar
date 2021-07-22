############################################################
### LIBRARY CALLS                                        ###
############################################################

library(ggplot2)


############################################################
### GENERATE PREDICTIONS                                 ###
############################################################

# starting at 200 this takes 58 minutes
aa <- Sys.time()
bikes_sv <- bikes_svbvar(
    log_scale = TRUE,
    agc = list(1, 200, FALSE)
)
Sys.time() - aa

save(bikes_sv, file = "data-raw/bikes_sv.RData")

############################################################
### SANITY CHECK, COMPARING WITH LM()                    ###
############################################################

load("data-raw/bikes_sv.Rdata")

df <- data.frame(
    y = bikes_d_log$logcnt[-c(1:200)],
    fitted = lm(logcnt ~ . -t, data = bikes_d_log)$fitted[-c(1:200)],
    sv_fitted = bikes_sv$pmean,
    time = bikes_d_log$t[201:length(bikes_d_log$t)],
    group = rep(1:5, each = 106)
)


plt <- ggplot(df, aes(y = y, x = time)) +
    geom_line() +
    geom_line(aes(y = fitted), col = "red") +
    geom_line(aes(y = sv_fitted), col = "blue") +
    facet_wrap(~group, ncol = 1, scales = "free") +
    labs(
        title = "Log counts and predictive means (stochvol) (red is fitted lm)",
        x = "Time",
        y = "Log count"
    )

ggsave("temp/stochvol.pdf", plt)

# NOTES about debugging
# at i = 364 (which predicts for t = 365) the variance of the predictive
# distribution totally explodes