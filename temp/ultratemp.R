# step 1, get svbvar up and running
# then the bart
# then try doing some aggregate methods
# is all good ppl

covariates <- bikes_d[
    ,
    .(
        yr,
        mnth,
        workingday,
        weather_1,
        weather_2,
        temp,
        hum,
        windspeed,
        sandy1,
        sandy2,
        cnt_l
    )
]


sv_pred <- bikes_svbvar(covariates)
save(sv_pred, file = "data-raw/sv_pred.Rdata")

abel <- Sys.time()
bart_pred <- bikes_bart(covariates)
Sys.time() - abel


a <- rnorm(10)
b <- rnorm(10)
c <- data.frame(a, b)
d <- as.matrix(c)
typeof(c)
typeof(d)


str(est_mod)

# The simple regression model

head(bikes_d)
m <- lm(cnt ~ . -logcnt -logcnt_l -t , data = bikes_d)
summary(m)
hist(m$residuals, breaks = 100)

# on the logscale then

ml <- lm(logcnt ~ . -cnt -logcnt_l -t , data = bikes_d)
summary(ml)
hist(ml$residuals, breaks = 100)

library(rstanarm)



m_bayes<- stan_glm(cnt ~ ., data = bikes_d[1:729, ], seed=111)
summary(m_bayes)

post_pred_draws <- posterior_predict(m_bayes, bikes_d[730, ])

btest <- bikes_regression(agc = list(1, 695, TRUE))


df <- data.frame(x = rnorm(10), y = rnorm(10))
df2 <- df
df <- subset(df, select = -c(x))
class(df)


df <- data.table(x = rnorm(5), y = rnorm(5))
df2 <- df


df2 <- df2[, .(y2 = y*2)]
