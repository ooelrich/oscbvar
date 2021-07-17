library(ggplot2)
head(bikes_d_log)


# Fit a simple linear regression
m1 <- lm(
    logcnt ~ . -t,
    data = bikes_d_log
)

df <- data.frame(
    y = bikes_d_log$logcnt,
    fitted = m1$fitted,
    time = bikes_d_log$t,
    group = rep(1:5, each = 146)
)


ggplot(df, aes(y = y, x = time)) +
    geom_line() +
    geom_line(aes(y = fitted), col = "red") +
    #facet_wrap(~group, ncol = 1, scales = "free") +
    labs(
        title = "Log counts and fitted values (lm)",
        x = "Time",
        y = "Log count"
    )