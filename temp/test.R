library(devtools)
load_all()


# Vignette nr 1: generate atomic predictions
menu <- gen_atomic_list()
list_of_models <- menu[c(2, 6, 9, 10)]
atomic_settings <- list(5, 60, FALSE)
adf <- gen_atomic_preds(menu, atomic_settings)
aa <- Sys.time()
Sys.time() - aa


# The four notebooks and making sure they are working correctly

atomic_settings <- list(5, 60, FALSE, 3)

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
