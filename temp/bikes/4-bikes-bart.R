############################################################
### LIBRARY CALLS                                        ###
############################################################

library(ggplot2)


############################################################
### GENERATE PREDICTIONS                                 ###
############################################################

# starting at 200 this takes 58 minutes
aa <- Sys.time()
bikes_bart <- bikes_bart(
    agc = list(1, 200, FALSE),
    include_intercept = FALSE,
    nrep = 10000,
    nburn = 5000
)
Sys.time() - aa

save(bikes_bart, file = "data-raw/bikes_bart.RData")


############################################################
### SANITY CHECK, COMPARING WITH LM()                    ###
############################################################

load("data-raw/bikes_bart.Rdata")

df <- data.frame(
    y = bikes_d_log$logcnt[-c(1:200)],
    fitted = lm(logcnt ~ . -t, data = bikes_d_log)$fitted[-c(1:200)],
    bart_fitted = bikes_bart$pmean,
    time = bikes_d_log$t[201:length(bikes_d_log$t)],
    group = rep(1:5, each = 106)
)


ggplot(df, aes(y = y, x = time)) +
    geom_line() +
    geom_line(aes(y = fitted), col = "red") +
    geom_line(aes(y = bart_fitted), col = "blue") +
    facet_wrap(~group, ncol = 1, scales = "free") +
    labs(
        title = "Log counts and fitted values (bart)",
        x = "Time",
        y = "Log count"
    )
