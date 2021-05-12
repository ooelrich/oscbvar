library(devtools)
load_all()


# Vignette nr 1: generate atomic predictions
menu <- gen_atomic_list()
list_of_models <- menu[c(2, 6, 9, 10)]
atomic_settings <- list(5, 60, FALSE, 1)
atomdat_1 <- gen_atomic_preds(list_of_models, atomic_settings)

save(atomdat_1, file = "temp/atomdat_1.Rdata")



start_t <- 5
sotw <- macrodata[start_t:nrow(macrodata), ]
sotw <- data.frame(t = c(1:nrow(sotw)), sotw)


weight_df <- caliper_relevance(
    atomdat_3,
    sotw,
    60,
    100,
    "full"
)