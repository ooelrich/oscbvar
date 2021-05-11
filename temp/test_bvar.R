library(devtools)
load_all()


atomic_settings <- list(5, 60, FALSE, 1)

# bvar_7
bvar7preds <- nb_bvar(
     macrodata[, 1:7],
     atomic_settings,
     lags = 1
)


par(mfrow = c(3,1))
plot(
    bvar_resp3$t,
    bvar_resp3$pmean - macrodata[65:218, 3],
    type = "l"
)

plot(
    bvar_resp2$t,
    bvar_resp2$pmean - macrodata[65:218, 2],
    type = "l"
)

plot(
    bvar_resp1$t,
    bvar_resp1$pmean - macrodata[65:218, 2],
    type = "l"
)



save(bvar_resp1, file = "data/bvar_resp1.RData")
save(bvar_resp2, file = "data/bvar_resp2.RData")
save(bvar_resp3, file = "data/bvar_resp3.RData")
