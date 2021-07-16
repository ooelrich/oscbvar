bikes_d <- read.csv(file = "data-raw/bikesharing/day.csv")

####################################
### HURRICANE SANDY CODING       ###
####################################
bikes_d[665:672, c("dteday", "cnt")]
# Sandy formed october 22, went "post-tropical" october 29th, and 
# dissipated november 2. Not clear which days we should "dummy out" as
# Sandy days. I think the data is from Washington, D.C.. Makes sense
# that people were a bit scared/not really biking around that much the
# day or so BEFORE the storm hit as well.
# Dummying out the 29th and the 30th atm.
bikes_d$sandy1 <- 0
bikes_d$sandy2 <- 0
bikes_d[bikes_d$dteday == "2012-10-29", "sandy1"] <- 1
bikes_d[bikes_d$dteday == "2012-10-30", "sandy2"] <- 1


#################################
### RECODE WEATHER TO DUMMIES ###
#################################

bikes_d$weather_1 <- 0
bikes_d$weather_2 <- 0
bikes_d[bikes_d$weathersit == 1, "weather_1"] <- 1
bikes_d[bikes_d$weathersit == 2, "weather_2"] <- 1


#################################
### DROP UNUSED VARUABLES     ###
#################################

bikes_d <- subset(
    bikes_d, 
    select = 
        -c(dteday,
        season,
        atemp,
        casual,
        registered,
        weekday,
        holiday,
        weathersit
    )
)


###################################
### CREATE LAGGED CNT           ###
##  REORDER COLUMNS             ###
###################################

cnt_l <- bikes_d$cnt[-nrow(bikes_d)]
names(bikes_d)[names(bikes_d)=="instant"] <- "t"
bikes_d <- bikes_d[, c(8, 1:7, 9:12)]
bikes_d <- cbind(bikes_d[-1, ], cnt_l)
bikes_d$t <- bikes_d$t - 1
row.names(bikes_d) <- 1:nrow(bikes_d)


###############################
### CREATE LOGGED VERSION   ###
###############################

logcnt <- log(bikes_d$cnt)
logcnt_l <- log(bikes_d$cnt_l)
bikes_d_log <- cbind(logcnt, bikes_d[, -c(1,13)], logcnt_l)
row.names(bikes_d_log) <- 1:nrow(bikes_d_log)

save(bikes_d, file = "data/bikes_d.RData")
save(bikes_d_log, file = "data/bikes_d_log.RData")