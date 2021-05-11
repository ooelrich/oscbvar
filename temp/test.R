library(devtools)
load_all()


# Vignette nr 1: generate atomic predictions
menu <- gen_atomic_list()
list_of_models <- menu[c(2, 6, 9, 10)]
atomic_settings <- list(5, 60, FALSE, 3)

# Generate all the atomic data
atomdat_3 <- gen_atomic_preds(list_of_models, atomic_settings)





# bvar_7
bvar7preds <- nb_bvar(
     macrodata[, 1:3],
     atomic_settings,
     lags = 1
)
head(bvar7preds)

# svbvar_7
svbvar <- nb_svbvar(
    macrodata[, 1:3],
    atomic_settings
)
head(svbvar)

# bart_7
aa <- Sys.time()
bart <- nb_bart(
    macrodata[, 1:7],
    atomic_settings
)
Sys.time() - aa
head(bart)


# tvpsvbvar
aa <- Sys.time()
tvpsvbvar <- nb_tvpsvbvar(
    macrodata[, 1:3],
    atomic_settings
)
Sys.time() - aa
head(tvpsvbvar)
dim(tvpsvbvar)
tvpsvbvar_resp3 <- tvpsvbvar
save(tvpsvbvar_resp3, file = "data/tvpsvbvar_resp3.RData")

# spara tvpsvbvar3_resp3 TAKES FOR EVER


nrow(macrodata)
plot(tvpsvbvar_resp3$t, tvpsvbvar_resp3$pmean, type = "l")
lines(tvpsvbvar_resp3$t, macrodata[65:218, 1], col = "red")

plot(tvpsvbvar_resp3$t, macrodata[65:218, 1], type = "l")

length(tvpsvbvar_resp3$t)
length(macrodata[65:218, 1])

class(macrodata)
head(macrodata)

bla <- TRUE
bla <- 3

if (!bla) {
    3
}