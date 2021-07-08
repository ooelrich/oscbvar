library(data.table)

bikes_d <- read.csv(file = "data-raw/bikesharing/day.csv")

bikes_d <- data.table(bikes_d)
bikes_d$weekday <- as.factor(bikes_d$weekday)
bikes_d$weathersit <- as.factor(bikes_d$weathersit)
bikes_d$holiday <- as.factor(bikes_d$holiday)
bikes_d$workingday <- as.factor(bikes_d$workingday)
bikes_d[, c("dteday","season", "atemp", "casual", "registered"):= NULL]
bikes_d$logcnt <- log(bikes_d$cnt)
cnt_l <- bikes_d$cnt[-nrow(bikes_d)]
logcnt_l <- bikes_d$logcnt[-nrow(bikes_d)]
bikes_d <- cbind(bikes_d[-1, ], cnt_l, logcnt_l)
names(bikes_d)[names(bikes_d)=="instant"] <- "t"

save(bikes_d, file = "data/bikes_d.RData")