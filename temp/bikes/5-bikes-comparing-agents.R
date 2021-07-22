############################################################
### COMPARING METHODS                                    ###
############################################################

df_bayes <- rbind(bikes_bart, bikes_sv, bikes_reg)

# predictive mean, including a freq lm model
df <- cbind(df_bayes, group = rep(1:5, each = 106))
ggplot(df, aes(y = pmean, x = t, color = method)) +
    geom_line() +
    geom_line(
        aes(y = ytrue, x = t),
        linetype = "dashed",
        color = "yellow",
        size = 1
    ) +
    facet_wrap( ~ group, ncol = 1, scales = "free") +
    labs(
        title = "All methods, yellow is the truth",
        x = "time",
        y = "pmean"
    )
ggsave("temp/all-pmean.pdf")

# log predictive density
ggplot(df, aes(y = lpdens, x = t, col = method)) +
    geom_line() +
    facet_wrap( ~ group, ncol = 1, scales = "free") +
    labs(
        title = "Log predictive densities",
        x = "time",
        y = "lpdens"
    )
ggsave("temp/all-lpdens.pdf")
