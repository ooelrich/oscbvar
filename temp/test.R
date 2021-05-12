library(devtools)
load_all()


# Vignette nr 1: generate atomic predictions
menu <- gen_atomic_list()
list_of_models <- menu[c(2, 6, 9, 10)]
atomic_settings <- list(5, 60, FALSE, 1)
atomdat_1 <- gen_atomic_preds(list_of_models, atomic_settings)

save(atomdat_1, file = "temp/atomdat_1.Rdata")


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

par(mfrow = c(1, 3))
plot(sotw$ussurv10, sotw$GDPC1)
plot(sotw$ussurv10, sotw$GDPCTPI)
plot(sotw$ussurv10, sotw$FEDFUNDS)
par(mfrow = c(1,1))

weight_df <- caliper_relevance(
    atomdat_3,
    sotw,
    161,
    5,
    10
)
View(weight_df)
