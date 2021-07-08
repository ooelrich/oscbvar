

library(data.table)

summary(lm(DTbikes$cnt ~ DTbikes$months + as.factor(DTbikes$season)))
summary(lm(DTbikes$cnt ~ DTbikes$months))
summary(lm(log(DTbikes$cnt) ~ DTbikes$months))

plot(DTbikes$months, DTbikes$cnt)
plot(DTbikes$months, log(DTbikes$cnt))
table(DTbikes$season)

# seasonal model
seas <- lm(DTbikes$cnt ~ DTbikes$months + as.factor(DTbikes$season))
plot(DTbikes$months, DTbikes$cnt)





m1 <- lm(
    cnt ~ .-t - logcnt - logcnt_l - atemp -casual - registered -weekday -months -holiday,
    data = bikes_d
)


m2 <- lm(
    logcnt ~ .-instant  -cnt -cnt_l - atemp -casual - registered -weekday -months,
    data = bikes_d
)

hist(m1$residuals)
hist(m2$residuals)
ts.plot(m1$residuals)
ts.plot(m2$residuals)
shapiro.test(m1$residuals)
shapiro.test(m2$residuals)