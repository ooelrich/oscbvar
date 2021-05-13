library(devtools)
load_all()

varnames <- c(
    "GDPC1",
    "GDPCTPI",
    "FEDFUNDS",
    "PCECC96",
    "GPDIC1",
    "HOANBS",
    "AHETPIx",
    "ussurv10"
)

colnames(dmdata) <- varnames
head(dmdata)
dmdata$ussurv10 <- ussurv/sqrt(var(ussurv))

dmdata[, "ussurv10"] <-  dmdata[, "ussurv10"]/sqrt(var(dmdata[, "ussurv10"]))


save(sotw, file = "temp/sotw.RData")
head(dmdata)

start_t <- 5
sotw <- dmdata[start_t:nrow(dmdata), ]
sotw <- data.frame(t = c(1:nrow(sotw)), sotw)
