library(ggplot2)
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



weight_df <- caliper_relevance(
    atomdat_3,
    sotw,
    161,
    5,
    10
)

View(weight_df)

head(sotw)


aggpred_data <- gen_agg_preds(
    atomdat_2,
    start_agg = 161,
    sotw,
    baseline = TRUE,
    caliper = TRUE,
    mahala  = FALSE,
    cw = 5,
    mvc = 10
)

aggpred_2 <- aggpred_data

save(aggpred_2, file = "data/aggpred_2.RData")

View(atomdat_3)
View(aggpred_data)


# Doing the old classics
lpdens_agg <- ggplot(aggpred_3, aes(y = lpdens, x = t, col = method)) +
    geom_line() +
    labs(title = "predictive density for aggregate methods")
ggsave("figs/lpdens_agg_fin_3.pdf", lpdens_agg)

aggpred_3[, .(lpdens_sum = sum(lpdens)), by = .(method)][order(lpdens_sum), ]

lpdens_agg <- ggplot(aggpred_2, aes(y = lpdens, x = t, col = method)) +
    geom_line() +
    labs(title = "predictive density for aggregate methods")
ggsave("figs/lpdens_agg_fin_2.pdf", lpdens_agg)

aggpred_2[, .(lpdens_sum = sum(lpdens)), by = .(method)][order(lpdens_sum), ]








