library(devtools)
load_all()


load("data/dmdat.Rdata")


# Script that generates a data set of pooling variables for the DM
# t needs to be in the first column, the rest are arbitrary
start_t <- 5
dmdm <- window(dmdat[, 1], start = min(time(macrodata)), end = max(time(macrodata)))
dmdm <- dmdm/sqrt(var(dmdm))
pooling_vars <- cbind(macrodata, dmdm)

varnames <- c(
    "GDPC1",
    "GDPCTPI",
    "FEDFUNDS",
    "PCECC96",
    "GPDIC1",
    "HOANBS",
    "AHETPIx",
    "USSURV1055"
)
colnames(pooling_vars) <- varnames

head(pooling_vars)
pooling_vars <- pooling_vars[-c(1:(start_t - 1)), ]
pooling_vars <- data.frame(t = c(1:nrow(pooling_vars)), pooling_vars)

save(pooling_vars, file = "temp/pooling_vars.Rdata")
