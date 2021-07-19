############################################################
### COMPARING METHODS                                    ###
############################################################

bikes_lm <- bikes_linreg(TRUE, list(1, 200, TRUE))
df_bayes <- rbind(bikes_bart, bikes_sv, bikes_reg)

# predictive mean, including a freq lm model
df <- cbind(rbind(bikes_all, bikes_lm), group = rep(1:5, each = 106))
ggplot(df, aes(y = pmean, x = t, color = method)) +
    geom_line() +
    geom_line(aes(y = ytrue, x = t), linetype = "dashed", color = "yellow", size = 1) +
    facet_wrap( ~ group, ncol = 1, scales = "free")

# log predictive density
df <- cbind(df_bayes, group = rep(1:5, each = 106))
ggplot(df, aes(y = lpdens, x = t, col = method)) +
    geom_line() +
    facet_wrap( ~ group, ncol = 1, scales = "free")
