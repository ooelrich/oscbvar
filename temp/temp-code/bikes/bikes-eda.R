
head(bikes_d)


# fit simple linear regression


m2 <- lm(
    logcnt ~ . -t  -cnt -cnt_l -weekday,
    data = bikes_d
)

hist(m1$residuals, breaks = 100)
hist(m2$residuals, breaks = 100)

ts.plot(m1$residuals)
ts.plot(m2$residuals)

shapiro.test(m1$residuals)
shapiro.test(m2$residuals)