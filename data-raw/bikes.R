library(data.table)

bikes_d <- read.csv(file = "data-raw/bikesharing/day.csv")

# Hurricane sandy
bikes_d[665:672, c("dteday", "cnt")]
# something happens here
# sandy formed october 22, went "post-tropical" october 29th, and 
# dissipated november 2
# Not clear which days we should "dummy out" as Sandy days. 
# I think the data is from Washington, D.C.. Makes sense that people
# were a bit scared/not really bikeing around that much the day or so 
# BEFORE the storm hit as well.

# Dummying out the 29th for sure, probably the 30th as well
# How about the 28th?

bikes_d <- data.table(bikes_d)
bikes_d$sandy1 <- 0
bikes_d$sandy2 <- 0
bikes_d$sandy1[668] <- 1
bikes_d$sandy2[669] <- 1

bikes_d$weather_1 <- 0
bikes_d$weather_2 <- 0
bikes_d[bikes_d$weathersit == 1, "weather_1"] <- 1
bikes_d[bikes_d$weathersit == 2, "weather_2"] <- 1

bikes_d[
    ,
    c(
        "dteday",
        "season",
        "atemp",
        "casual", 
        "registered",
        "weekday",
        "holiday",
        "weathersit"
    ):= NULL
]

cnt_l <- bikes_d$cnt[-nrow(bikes_d)]
names(bikes_d)[names(bikes_d)=="instant"] <- "t"
bikes_d$t <- bikes_d$t
bikes_d <- bikes_d[, c(8, 1:7, 9:12)]
bikes_d <- cbind(bikes_d[-1, ], cnt_l)
bikes_d$t <- bikes_d$t - 1

save(bikes_d, file = "data/bikes_d.RData")

# logversion of data
logcnt <- log(bikes_d$cnt)
logcnt_l <- log(bikes_d$cnt_l)
bikes_d_log <- cbind(logcnt, bikes_d[, -c(1,13)], logcnt_l)

save(bikes_d_log, file = "data/bikes_d_log.RData")