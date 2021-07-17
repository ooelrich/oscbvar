############################################################
### LIBRARY CALLS                                        ###
############################################################

library(ggplot2)


############################################################
### GENERATE PREDICTIONS                                 ###
############################################################

# starting at 200 this takes 24 minutes
aa <- Sys.time()
bikes_reg <- bikes_regression(
    log_scale = TRUE,
    agc = list(1, 200, FALSE)
)
Sys.time() - aa

save(bikes_reg, file = "data-raw/bikes_reg.RData")


############################################################
### SANITY CHECK, COMPARING WITH LM()                    ###
############################################################

load("data-raw/bikes_reg.Rdata")

df <- data.frame(
    y = oscbvar::bikes_d_log$logcnt[-c(1:200)],
    fitted = lm(logcnt ~ .-t, data = oscbvar::bikes_d_log)$fitted[-c(1:200)],
    reg_fitted = bikes_reg$pmean,
    time = oscbvar::bikes_d_log$t[201:length(oscbvar::bikes_d_log$t)],
    group = rep(1:5, each = 106)
)


ggplot(df, aes(y = y, x = time)) +
    geom_line() +
    geom_line(aes(y = fitted), col = "red") +
    geom_line(aes(y = reg_fitted), col = "blue") +
    facet_wrap(~group, ncol = 1, scales = "free") +
    labs(
        title = "Log counts and fitted values (b-reg)",
        x = "Time",
        y = "Log count"
    )
