library(data.table)

bikes_d <- read.csv(file = "data-raw/bikesharing/day.csv")

# Hurricane sandy
bikes_d[665:672, c("dteday", "cnt", "sandy1", "sandy2")]
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