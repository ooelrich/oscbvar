library(devtools)
load_all()

## Detta är kod som generarar default atomiska data (alltså med den setup vi använder i pappret), som även används i våra vignetter.

# Vignette nr 1: generate atomic predictions
menu <- gen_atomic_list()
list_of_models <- menu[c(2, 6, 9, 10)]
atomic_settings <- list(5, 60, FALSE, 3)
atomdat_3 <- gen_atomic_preds(list_of_models, atomic_settings)

save(atomdat_3, file = "data/atomdat_3.RData")
save(atomdat_2, file = "data/atomdat_2.RData")
save(atomdat_1, file = "data/atomdat_1.RData")