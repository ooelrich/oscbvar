load_all()

plot(
    bvar_resp3$t,
    tvpsvbvar_resp3$pmean - macrodata[65:218, 3],
    type = "l"
)


save(tvpsvbvar_resp3, file = "data/tvpsvbvar_resp3.RData")
