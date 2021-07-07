# Bikesharing data in hourly and daily formats
bikes_d <- read.csv(file = "data-raw/bikesharing/day.csv")
bikes_h <- read.csv(file = "data-raw/bikesharing/hour.csv")

# Let's reverse engineer how the daily version is derived from the
# hourly version

# temperature is the average temperature over the whole day
# (a really cold night would probably not influence the renting
# behaviour if the day is sunny and warm)
mean(bikes_h[1:24, "temp"])
bikes_d[1, "temp"]

# We have a categorical weather variable
# The aggregate appears to have been caluculated as a simple mean
# Clearly it's not the mode, as can be seen when looking at the first
# day
table(bikes_h[1:24, "weathersit"])
bikes_d[1, "weathersit"]
mean(bikes_h[1:24, "weathersit"])

dim(bikes)
View(bikes)
head(bikes)

# For days with missing hours the temp is still the mean of temp
# so it's not actually the daily average anymore
# This is machinelearning for sure...
mean(bikes_h[25:47, "temp"])
bikes_d[2, "temp"]

# 1 is winter
ggplot(bikes, aes(y = cnt, x = season)) + geom_col()

# 1 is "best" weather, 4 is "worst"
ggplot(bikes, aes(y = cnt, x = weathersit)) + geom_col()

ggplot(bikes, aes(y = cnt, x = holiday)) + geom_col()

# Here is where we save the stuff after having processed it
# usethis::use_data()

head(bikes$cnt)
var(bikes$cnt)
agc <- list(
    5,
    100,
    rolling = FALSE,
    1
)

df_bikes <- data.frame(
    countz = bikes$cnt/sqrt(var(bikes$cnt)),
    t = seq(1, length(bikes$cnt))
)



haha <- nb_bvar(df_bikes, agc, lags = 1)

# "känns som temp" har sämre förklaringsgrad än temperatur!!
# (inte så stor skillnad)
summary(lm(bikes$cnt ~ bikes$temp))
summary(lm(bikes$cnt ~ bikes$atemp))


library(data.table)

DTbikes <- data.table(bikes_d)
DTbikes[, months := mnth + yr * 12] # not sure...

summary(lm(DTbikes$cnt ~ DTbikes$months + as.factor(DTbikes$season)))
summary(lm(DTbikes$cnt ~ DTbikes$months))
summary(lm(log(DTbikes$cnt) ~ DTbikes$months))

plot(DTbikes$months, DTbikes$cnt)
plot(DTbikes$months, log(DTbikes$cnt))
table(DTbikes$season)

# seasonal model
seas <- lm(DTbikes$cnt ~ DTbikes$months + as.factor(DTbikes$season))
plot(DTbikes$months, DTbikes$cnt)

# rename instant as t
# don't really need to drop stuff I guess